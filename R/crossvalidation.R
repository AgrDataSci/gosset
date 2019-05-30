#' Cross-validation 
#'
#' Methods for measuring the performance of a predictive model on sets of test data. 
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted,
#' of the form \eqn{ y ~ x1 + ... + xn }
#' @param data a data frame, list or environment
#'(or object coercible by as.data.frame to a data frame) 
#' containing the variables in the model.
#' @param k an integer for the number of splits in the cross-validation
#' @param folds an optional vector specifying the folds in the cross-validation
#' @param mean.method a character for the method to calculate the mean of cross-validation estimators. 
#' Options are: 'equal', an arithmetic mean; 
#' 'foldsize', a weighted mean by the size in each fold; 
#' 'stouffer' a weighted Z-test developed by Stouffer et al. (1949). 
#' The two last methods are suggested for cross-validation with imbalanced folds
#' @param ... additional arguments passed to methods
#' @return The cross-validation goodness-of-fit estimates for the best model, which are:
#' \item{AIC}{Akaike Information Criterion}
#' \item{deviance}{Model deviance}
#' \item{logLik}{Log-Likelihood}
#' \item{MaxLik}{Maximum likelihood pseudo R-squared}
#' \item{CraggUhler}{Cragg and Uhler's pseudo R-squared}
#' \item{Agresti}{Agresti pseudo R-squared}
#' Cross-validation estimates are computed using the fitted models on the validation samples.
#' @seealso \code{\link[gnm]{gnm}}, \code{\link[PlackettLuce]{pltree}}, 
#' \code{\link[psychotree]{bttree}}
#' @examples
#' \dontrun{
#' 
#' # Generalized Linear Models
#' 
#' data("airquality")
#' 
#' crossvalidation(Temp ~ Wind + Solar.R,
#'                 data = airquality,
#'                 k = 5,
#'                 family = poisson())
#' 
#' ########################################
#' 
#' # PlackettLuce Model
#' # beans data from PlackettLuce
#' library("PlackettLuce")
#' 
#' example("beans", package = "PlackettLuce")
#' 
#' G <- grouped_rankings(R, rep(seq_len(nrow(beans)), 4))
#' 
#' beans <- cbind(G, beans)
#' 
#' # take seasons as bins
#' k <- length(unique(beans$season))
#' folds <- as.integer(as.factor(beans$season))
#' 
#' crossvalidation(G ~ maxTN, 
#'                 data = beans, 
#'                 k = k, 
#'                 folds = folds, 
#'                 minsize = 100)
#' 
#' ########################################
#' 
#' # BradleyTerry Model
#' library("psychotree")
#' # Germany's Next Topmodel 2007 data from psychotree
#' data("Topmodel2007", package = "psychotree")
#' 
#' crossvalidation(preference ~ ., 
#'                 data = Topmodel2007,
#'                 k = 5)
#'                 
#' }
#'                 
#' @import partykit
#' @import psychotools
#' @import psychotree
#' @import PlackettLuce
#' @import gnm
#' @export
crossvalidation <- function(formula, data, k = NULL,
                            folds = NULL, mean.method = NULL,
                            ...)
{
  
  # get dots for extra arguments
  dots <- list(...)
  
  # create folds if needed, check length if given
  n <- nrow(data)
  
  # 10 folds by default
  if (is.null(k)) {
    k <- 10
  }
  
  if (is.null(folds)) {
    folds <- sample(rep(1:k, times = ceiling(n / k), length.out = n))
  }
  
  if (length(folds) != n) {
    stop("folds and nrow(data) has different length")
  }
  
  # validate mean.method
  mean.opt <- c("stouffer", "foldsize", "equal")
  
  if (is.null(mean.method)) {
    mean.method <- mean.opt[1]
  }
  
  if (!mean.method %in% mean.opt) {
    stop("unknown mean method, valid options are: ", 
         paste(mean.opt, collapse = ", "), "\n")
  }
  
  # create a model frame 
  environment(formula) <- parent.frame()
  
  data <- model.frame(formula, data)
  
  Y <- model.response(data)
  
  # define the most suitable model based on the 
  # class of the response variable
  model <- "gnm"
  
  if (.is_grouped_rankings(Y)) {model <- "pltree"}
  
  if (.is_paircomp(Y)) {model <- "bttree"}
  
  # split data into lists with training and test set
  train <- list()
  for (i in 1:k) {
    train[[i]] <- data[folds != i ,]
  }
  
  test <- list()
  for (i in 1:k) {
    test[[i]] <- data[folds == i  ,]
  }
  
  # fit the models
  mod <- lapply(train, function(X) {
      
      args <- list(formula = formula, data = X)
      
      args <- c(args, dots)
      
      do.call(model, args)
      
    })
  
  # take models from train data to compute deviance, pseudo R-squared
  # and the predictions of the test part of the data
  aic <- mapply(function(X, Y) {
    AIC(X, newdata = Y)
  }, X = mod, Y = test[])
  
  Deviance <- mapply(function(X, Y) {
    deviance(X, newdata = Y)
  }, X = mod, Y = test[])
  
  logLik <- Deviance / -2
  
  pR2 <- t(mapply(function(X) {
    pseudoR2(X)[]
  }, X = mod))
  
  pR2 <- matrix(unlist(pR2), 
                ncol = 5, 
                nrow = k, 
                dimnames = list(1:k, dimnames(pR2)[[2]]))
  
  # no need to keep null logLik
  pR2 <- pR2[, -c(1,2)]
  
  estimators <- cbind(AIC = aic, 
                      deviance = Deviance, 
                      logLik = logLik, 
                      pR2)
  
  # estimators are then averaged weighted by 
  # number of predicted cases using selected mean method
  means <- apply(estimators, 2, function(x) {
      .mean_crossvalidation(object = x, 
                            folds = folds, 
                            mean.method = mean.method)
    })
  
  # means and estimates as tibble
  means <- tibble::as_tibble(t(means))

  estimators <- tibble::as_tibble(estimators)
  
  # get predictions for these datasets
  preds <- mapply(function(X,Y) {
    predict(X, newdata = Y)
  }, X = mod, Y = test[])
  
  
  
  result <- list(coeffs = means,
                 raw = list(call = deparse(formula, width.cutoff = 500),
                            estimators = estimators,
                            folds = folds,
                            models = mod,
                            predictions = preds,
                            data = data))
  
  class(result) <- c("crossvalidation", class(result))
  return(result)
}

#' @method print crossvalidation
#' @export
print.crossvalidation <- function(x, ...) {
  cat("Model formula:\n")
  cat(x[[2]][[1]], "\n \n")
  cat("Cross-validation estimates: \n")
  print(x[[1]])
}

# Compute weighted means in cross-validation
.mean_crossvalidation <- function(object, folds = NULL, 
                                  mean.method = NULL, 
                                  ...){
  # take length of folds
  N <- length(folds)
  
  if (is.null(mean.method)) {mean.method <- "stouffer"}
  
  # weight of imbalanced folds
  if (mean.method == "stouffer") {
    # take the number of folds
    max_folds <- max(folds)
    
    # make a table of folds and take
    # how many observations each fold has
    foldsize <- table(folds)
    
    # take the weight of each fold
    # first, the squared root of foldsize (observations per fold)
    # by the total number of observation
    wfold <- sqrt(as.vector(foldsize) / N)
    
    # then divide this vector by its sum
    wfold <- wfold / sum(wfold)
    # wfold <- (max_folds * wfold) / sum(wfold)
    
    # then we multiply the input values by the
    # weight of each fold
    stouffer <- object * wfold
    
    # sum these values and that is the stouffer mean
    mean <- sum(stouffer)
  }
  
  if (mean.method == "foldsize") {
    # make a table of folds and take
    # the number of observations per fold
    foldsize <- as.vector(table(folds))
    
    # fold size mean is the product of multiplication of object values by 
    # its number of observations then divided by the total number of observations
    mean <- sum(object * foldsize, na.rm = TRUE) / sum(foldsize, na.rm = TRUE)
  }
  
  if (mean.method == "equal") {
    mean <- mean(object, na.rm = TRUE)
  }
  
  return(mean)
  
}


# Test a grouped_rankings object
.is_grouped_rankings <- function(object) {
  
  return(class(object) == "grouped_rankings")
  
}

# Test a rankings object
.is_rankings <- function(object) {
  
  return(class(object) == "rankings")
  
}

# Test a paircomp object
.is_paircomp <- function(object) {
  
  return(class(object) == "paircomp")
  
}




