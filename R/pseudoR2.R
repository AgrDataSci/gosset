#' Pseudo R-squared
#' 
#' Regression coefficient to evaluate goodness-of-fit in a given model when 
#' ordinary least squares (OLS) are not available. The algorithm computes estimates 
#' from the maximum likelihood through an iterative process. These estimates are called 
#' 'pseudo R-squared' because they look like 'R-squared' in the sense that they are on 
#' a similar scale (from 0 to 1), with higher values indicating better model fit.
#' 
#' @author Kauê de Sousa and Jacob van Etten
#' @family goodness-of-fit functions
#' @param object a model object of class glm, gnm, lm, pltree or bttree
#' @param ... additional arguments affecting the R-squared produced
#' @return A data frame containing the pseudo R-squared coefficients:
#' \item{logLik}{log-likelihood}
#' \item{logLikNull}{Null log-likelihood}
#' \item{MaxLik}{maximum likelihood pseudo R-squared}
#' \item{CraggUhler}{Cragg and Uhler's pseudo R-squared}
#' \item{Agresti}{Agresti pseudo R-squared}
#' @references 
#' 
#' Agresti A. (2002). Categorical Data Analysis. John Wiley & Sons, Inc., 
#' Hoboken, NJ, USA. http://doi.wiley.com/10.1002/0471249688
#' 
#' Hunter D. R. (2004). The Annals of Statistics, 32(1), 384–406. 
#' http://www.jstor.org/stable/3448514
#' 
#' @examples
#'
#' data("airquality")
#'
#' mod <- glm(Temp ~ Wind + Solar.R,
#'            data = airquality,
#'            family = poisson())
#'
#' pseudoR2(mod)
#'
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @importFrom PlackettLuce PlackettLuce as.grouped_rankings
#' @importFrom stats deviance formula na.omit predict update
#' @export
pseudoR2 <- function(object, ...) {
  
  UseMethod("pseudoR2")
  
}

#' @rdname pseudoR2
#' @export
pseudoR2.default <- function(object, ...){
  
  dots <- list(...)
  
  newdata <- dots[["newdata"]]
  
  # update the model as a null model
  # model without covariates
  mod_null <- stats::update(object, ~ 1)
  
  # pseudo R squared
  if (is.null(newdata)) {
    # get the logLik of the null model
    LLNull <- stats::deviance(mod_null)[1] / -2
    
    # get the logLik of the original model
    LL <- stats::deviance(object)[1] / -2
    
    # number of observations in the original model
    # to avoid issues with the structure of different model aproaches
    # we get the number of rows of a prediction matrix which will
    # have the same dimensions as the response variable
    n <- nrow(as.matrix(stats::predict(object)))
  }
  
  # pseudo R squared on a validation sample
  if (!is.null(newdata)) {
    # get the null logLik using the validation sample
    LLNull <- stats::deviance(mod_null, newdata = newdata) / -2
    
    # get the logLik using the training model and
    # the validation set
    LL <- stats::deviance(object, newdata = newdata) / -2
    
    # number of observations in the original model
    # to avoid issues with the structure of different model approaches
    # we get the number of rows of a prediction matrix which will
    # have the same dimensions as the response variable
    n <- nrow(as.matrix(stats::predict(object, newdata = newdata)))
    
  }
  
  pR2 <- .getpseudoR2(LLNull, LL, n)
  
  return(pR2)
  
}

#' @rdname pseudoR2
#' @method pseudoR2 pltree
#' @export
pseudoR2.pltree <- function(object, ...){
  
  dots <- list(...)
  
  newdata <- dots[["newdata"]]
  
  # identify the name of response variable
  Y <- all.vars(stats::formula(object))[1]
  
  # pR2 in a fit sample
  if (is.null(newdata)) {
    #logLik of object
    LL <- stats::deviance(object)[1] / -2
    
    # observed rankings
    R <- object[[1]]$data
    R <- R[, Y]
    R <- R[1:length(R), , as.grouped_rankings = FALSE]
    
    # number of observations
    n <- nrow(object[[1]]$data)
    
    
  }
  
  # pR2 on a validation sample
  if (!is.null(newdata)) {
    # predicted logLik on newdata using object
    LL <- stats::deviance(object, newdata = newdata) / -2
    
    # observed rankings on newdata
    R <- newdata[, Y]
    R <- R[1:length(R), , as.grouped_rankings = FALSE]
    
    # number of observations in newdata
    n <- nrow(newdata)
    
  }
  
  # logLik of a null model
  LLNull <- PlackettLuce::PlackettLuce(R)
  LLNull <- LLNull$null.loglik

  pR2 <- .getpseudoR2(LLNull, LL, n)
  
  return(pR2)
  
}

#' @rdname pseudoR2
#' @method pseudoR2 bttree
#' @export
pseudoR2.bttree <- function(object, ...){
  
  dots <- list(...)
  
  newdata <- dots[["newdata"]]
  
  # get the response variable
  Y <- all.vars(stats::formula(object))[1]
  
  if (is.null(newdata)) {
    #logLik of object
    LL <- deviance(object)[1] / -2
    
    # observed rankings
    R <- object[[1]]$data
    R <- R[, Y]
    
    # get it as a matrix of rankings
    R <- PlackettLuce::as.grouped_rankings(R)
    R <- R[1:length(R), , as.grouped_rankings = FALSE]
    
    # number of observations
    n <- nrow(R)
    
    
  }
  
  if (!is.null(newdata)) {
    # predicted logLik on newdata using object
    LL <- stats::deviance(object, newdata = newdata) / -2
    
    # observed rankings on newdata
    R <- newdata[, Y]
    
    # get it as a matrix of rankings
    R <- PlackettLuce::as.grouped_rankings(R)
    R <- R[1:length(R), , as.grouped_rankings = FALSE]
    
    # number of observations in newdata
    n <- nrow(R)
    
  }
  
  # zeros into NA
  R[R == 0] <- NA
  
  # logLik of a null model
  LLNull <- .logLikNull(R)
  
  pR2 <- .getpseudoR2(LLNull, LL, n)
  
  return(pR2)
  
}

#' Compute log likelihood of a null ranking model
#' 
#' @param object a matrix with rankings
#' @return the null log likelihood
#' @examples 
#' R <- matrix(c(1, 2, NA, NA,
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, NA,
#'               2, 1, 1, NA,
#'               1, NA, 3, 2), 
#'             nrow = 6, byrow = TRUE)
#' 
#' .logLikNull(R)
#' @noRd
.logLikNull <- function(object) {
  
  dimo <- dim(object)
  
  if(is.null(dimo)){
    object <- as.matrix(t(object))
  }
  # This function assumes that all coefficients
  # are equal to get a true null estimates
  coeff <- rep(0, dim(object)[[2]])
  
  LL <- apply(object, 1, function(x) {
    # Put coefficients in the right order
    v <- as.vector(stats::na.omit(coeff[order(x, na.last = NA)]))
    l <- 0
    # From Hunter MM(2004) The Annals of Statistics, Vol. 32, 
    # No. 1, 384-406, page 397
    for (i in seq_along(v[-1])) {
      l <- l + v[i] - log(sum(exp(v[(i):(length(v))])))
    }
    return(l)
    
  })
  
  sum(LL)
  
}

#' Compute pseudo R squared
#'  
#' @param LLNull numeric, the null log likelihood
#' @param LL numeric, the log likelihood
#' @param n integer, the number of observations 
#' @return the pseudo R squared
#' @noRd 
.getpseudoR2 <- function(LLNull, LL, n) {
  # minus two times the logLik
  g2 <- ((LLNull - LL) * -2)
  # maximum likelihood pseudo R2
  maxlike <- 1 - exp(-g2 / n)
  # maximised maxlike
  maxlike_max <- 1 - exp(LLNull * 2 / n)
  # Cragg and Uhler's pseudo R2
  cu_pr2 <- maxlike / maxlike_max
  # Agresti pseudo R2
  agr_pr2 <- 1 - (LL / LLNull)
  
  result <- data.frame(
    logLik = LL,
    logLikNull = LLNull,
    MaxLik = maxlike,
    CraggUhler = cu_pr2,
    Agresti = agr_pr2
  )
  
  class(result) <- union("gosset_df", class(result))
  
  return(result)
  
}
