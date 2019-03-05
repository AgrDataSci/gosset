#' Pseudo R-squared 
#' 
#' Regression coefficient to evaluate goodness-of-fit in a given model  
#' 
#' @param object a model object for which pseudo R-squared is desired
#' @param ... additional arguments affecting the R-squared produced
#' @return A tidy object containing the pseudo R-squared coefficients:
#' \item{logLik}{log-likelihood from the fitted model}
#' \item{logLikNull}{log-likelihood from the null model}
#' \item{MaxLik}{maximum likelihood pseudo R-squared}
#' \item{CraggUhler}{Cragg and Uhler's pseudo R-squared}
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
#' #################################
#' 
#' # Compute pseudo R-squared on a validation sample
#' 
#' n <- nrow(airquality)
#' 
#' s <- sample(1:n, n * 0.7)
#' 
#' train <- airquality[s,]
#' test <- airquality[-s,]
#' 
#' mod <- glm(Temp ~ Wind + Solar.R,
#'            data = train,
#'            family = poisson())
#' 
#' pseudoR2(mod, newdata = test)
#' 
#' @export
pseudoR2 <- function(object, ...){
  
  UseMethod("pseudoR2")
  
}

#' @export
pseudoR2.default <- function(object, newdata = NULL){
  
  # update the model as a null model 
  # model without covariates 
  mod_null <- update(object, ~ 1)
  
  # pseudo R squared 
  if(is.null(newdata)){
    # get the logLik of the null model
    LLNull <- deviance(mod_null)[1] / -2
    
    # get the logLik of the original model
    LL <- deviance(object)[1] / -2
    
    # number of observations in the original model
    # to avoid issues with the structure of different model aproaches
    # we get the number of rows of a prediction matrix which will
    # have the same dimensions as the response variable 
    n <- nrow(as.matrix(predict(object)))
  }
  
  # pseudo R squared on a validation sample
  if(!is.null(newdata)){
    # get the null logLik using the validation sample 
    LLNull <- deviance(mod_null, newdata = newdata) / -2

    # get the logLik using the training model and
    # the validation set
    LL <- deviance(object, newdata = newdata) / -2
    
    # number of observations in the original model
    # to avoid issues with the structure of different model aproaches
    # we get the number of rows of a prediction matrix which will
    # have the same dimensions as the response variable 
    n <- nrow(as.matrix(predict(object, newdata = newdata)))
    
  }
  
  # minus two times the logLik
  g2 <- ((LLNull - LL) * -2)
  # maximum likelihood pseudo R2
  maxlike <- 1 - exp(-g2 / n)
  # maximised maxlike
  maxlike_max <- 1 - exp(LLNull * 2 / n)
  # Cragg and Uhler's pseudo R2
  cu_pr2 <- maxlike / maxlike_max
  
  result <- dplyr::bind_cols(logLik = LL, logLikNull = LLNull,
                             MaxLik = maxlike, CraggUhler = cu_pr2)
  
  return(result)
  
}

#' @method pseudoR2 pltree
#' @export
pseudoR2.pltree <- function(object, newdata = NULL){
  
  # get the response variable 
  Y <- all.vars(formula(object))[1]
  
  if (is.null(newdata)) {
    #logLik of object
    LL <- deviance(object)[1] / -2
    
    # observed rankings
    R <- object[[1]]$data
    R <- R[, Y]
    R <- R[1:length(R), , as.grouped_rankings = FALSE]
    
    # number of observations
    n <- nrow(object[[1]]$data)
    
    
  }
  
  if (!is.null(newdata)) {
    # predicted logLik on newdata using object
    LL <- deviance(object, newdata = newdata) / -2
  
    # observed rankings on newdata
    R <- newdata[, Y]
    R <- R[1:length(R), , as.grouped_rankings = FALSE]
    
    # number of observations in newdata
    n <- nrow(newdata)
  
  }
  
  # zeros into NA
  R[R == 0] <- NA
  
  # logLik of a null model 
  LLNull <- .logLikNull(R)

  # minus two times the logLik
  g2 <- ((LLNull - LL) * -2)
  # maximum likelihood pseudo R2
  maxlike <- 1 - exp(-g2 / n)
  # maximised maxlike
  maxlike_max <- 1 - exp(LLNull * 2 / n)
  # Cragg and Uhler's pseudo R2
  cu_pr2 <- maxlike / maxlike_max
  
  result <- dplyr::bind_cols(logLik = LL, logLikNull = LLNull,
                             MaxLik = maxlike, CraggUhler = cu_pr2)
  
  return(result)
  
}

#' @method pseudoR2 bttree
#' @export
pseudoR2.bttree <- function(object, newdata = NULL){
  
  # get the response variable 
  Y <- all.vars(formula(object))[1]
  
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
    LL <- deviance(object, newdata = newdata) / -2
    
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
  
  # minus two times the logLik
  g2 <- ((LLNull - LL) * -2)
  # maximum likelihood pseudo R2
  maxlike <- 1 - exp(-g2 / n)
  # maximised maxlike
  maxlike_max <- 1 - exp(LLNull * 2 / n)
  # Cragg and Uhler's pseudo R2
  cu_pr2 <- maxlike / maxlike_max
  
  result <- dplyr::bind_cols(logLik = LL, logLikNull = LLNull,
                             MaxLik = maxlike, CraggUhler = cu_pr2)
  
  return(result)
  
}

# Compute log likelihood of a null ranking model
.logLikNull <- function(object){
  
  # This function assumes that all coefficients
  # are equal to get a true null estimates
  coeff <- rep(0, dim(object)[[2]])
  
  LL <- apply(object, 1, function(x){
    # Put coefficients in the right order
    v <- as.vector(na.omit(coeff[order(x, na.last = NA)]))
    l <- 0
    # From Hunter MM(2004) The Annals of Statistics, Vol. 32, No. 1, 384-406, page 397
    for(i in 1:(length(v)-1)) l <- l + v[i] - log(sum(exp(v[(i):(length(v))])))
    
    return(l)
    
  })
  
  sum(LL)
  
}