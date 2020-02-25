#' Cross-validation 
#'
#' Methods for measuring the performance of a predictive model on sets of 
#' test data in Bradley-Terry model from \pkg{psychotree}, Generalized Linear and 
#' Generalized Nonlinear models from \pkg{gnm}, and Plackett-Luce model from 
#' \pkg{PlackettLuce}
#'
#' @family model selection functions
#' @param formula an object of class "formula" (or one that can be 
#' coerced to that class): a symbolic description of the model to be fitted,
#' of the form \eqn{y ~ x1 + ... + xn}
#' @param data a data frame (or object coercible by as.data.frame to a data frame)
#' containing the variables in the model
#' @param k an integer for the number of bins in the cross-validation
#' @param folds an optional vector specifying the folds in the cross-validation
#' @param mean.method a character for the method to calculate the mean of 
#' cross-validation estimators. 
#' Options are: 'equal', arithmetic mean; 
#' 'foldsize', weighted mean by the size in each fold; 
#' 'stouffer' weighted through Z-test. See references 
#' @param seed integer, the seed for random number generation. If NULL (the default), 
#' \pkg{gosset} will set the seed randomly
#' @param ... additional arguments passed the methods of the chosen model
#' @return an object of class \code{gosset_cv} with the cross-validation 
#' goodness-of-fit estimates, which are:
#' \item{AIC}{Akaike Information Criterion}
#' \item{deviance}{Model deviance}
#' \item{logLik}{Log-Likelihood}
#' \item{MaxLik}{Maximum likelihood pseudo R-squared}
#' \item{CraggUhler}{Cragg and Uhler's pseudo R-squared}
#' \item{Agresti}{Agresti pseudo R-squared}
#' \item{kendallTau}{the Kendall correlation coefficient, only for Plackett-Luce models}
#' @seealso \code{\link[psychotree]{bttree}}, 
#' \code{\link[gnm]{gnm}},
#' \code{\link[PlackettLuce]{pltree}}
#' @references 
#' Agresti A. (2002). Categorical Data Analysis. 
#' \url{http://doi.wiley.com/10.1002/0471249688}
#' 
#' Elder J. F. (2003). Journal of Computational and Graphical Statistics, 12(4), 853–864.
#' \url{https://doi.org/10.1198/1061860032733}
#' 
#' James G., et al. (2013). An Introduction to Statistical Learning: with Applications in R.
#' \url{https://doi.org/10.1007/978-1-4614-7138-7}
#' 
#' Whitlock M. C. (2005). Journal of Evolutionary Biology, 18(5), 1368–1373. 
#' \url{https://doi.org/10.1111/j.1420-9101.2005.00917.x}
#' 
#' @examples
#' # Generalized Linear Models
#' require("gnm")
#' 
#' data("airquality")
#' 
#' cv <- crossvalidation(Temp ~ Wind + Solar.R,
#'                       data = airquality,
#'                       k = 3,
#'                       seed = 999,
#'                       family = poisson())
#'                 
#' 
#' \donttest{
#' # Plackett-Luce Model
#' require("PlackettLuce")
#' 
#' # beans data from PlackettLuce
#' data("beans", package = "PlackettLuce")
#' 
#' G <- rank_tricot(data = beans,
#'                  items = c(1:3),
#'                  input = c(4:5),
#'                  additional.rank = beans[c(6:8)],
#'                  group = TRUE)
#' 
#' beans <- cbind(G, beans)
#' 
#' # take seasons as bins
#' k <- length(unique(beans$season))
#' folds <- as.integer(as.factor(beans$season))
#' 
#' cv <- crossvalidation(G ~ maxTN,
#'                       data = beans,
#'                       k = k,
#'                       folds = folds,
#'                       minsize = 100)
#' 
#' ########################################
#' 
#' # Bradley-Terry Model
#' require("psychotree")
#' 
#' # Germany's Next Topmodel 2007 data from psychotree
#' data("Topmodel2007", package = "psychotree")
#' 
#' cv <- crossvalidation(preference ~ .,
#'                       data = Topmodel2007,
#'                       k = 5)
#'                 
#' }
#'                 
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @importFrom stats model.frame model.response runif
#' @export
crossvalidation <- function(formula, 
                            data, 
                            k = 10,
                            folds = NULL, 
                            mean.method = NULL,
                            seed = NULL,
                            ...)
{
  
  # get dots for extra arguments
  dots <- list(...)
  
  # create folds if needed, check length if given
  n <- nrow(data)
  
  # assign folds
  if (is.null(folds)) {
    
    # check if a seed is provided
    if (is.null(seed)) {
      seed <- as.integer(stats::runif(1, 0, 10000))
    }
    
    set.seed(seed)
    
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
  
  data <- stats::model.frame(formula, data)
  
  Y <- stats::model.response(data)
  
  # define the most suitable model based on the 
  # class of the response variable
  model <- "gnm"
  
  if (.is_grouped_rankings(Y)) {
    
    model <- "pltree"
    
    message("Using Plackett-Luce model\n")
    
  }
  
  if (.is_paircomp(Y)) {
    
    model <- "bttree"
    
    message("Using Bradley-Terry model\n")
    
  }
  
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
    
    try(do.call(model, args))
    
  })
  
  # get goodness-of-fit estimates from models
  gof <- .get_estimators(model = mod, test_data = test)

  estimators <- gof[[1]]
  
  # if model is pltree take the kendall cor 
  if (model == "pltree") {
    
    R_pl <- all.vars(formula)[[1]]
    
    R_pl <- lapply(test, function(x) {
      
      x <- x[, R_pl]
      
      x[1:length(x), , as.grouped_rankings = FALSE]
      
    })
    
    preds <- gof[[2]]
    
    KT <- mapply(function(X, Y) {
      
        try(kendallTau(X, Y)[[1]], silent = TRUE)
      
      }, X = R_pl, Y = preds)
      
      KT <- as.numeric(KT)
      
      estimators$kendallTau <- KT
      
  }
  
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

  result <- list(coeffs = means,
                 raw = list(call = deparse(formula, width.cutoff = 500),
                            estimators = estimators,
                            folds = folds,
                            models = mod,
                            data = data))
  
  class(result) <- union("gosset_cv", class(result))
  
  return(result)
}


#' Get estimators from model parameters
#' 
#' @param model a list with models
#' @param test_data a dist with data.frames to test model
#' @return The model goodness-of-fit estimators
#' data("airquality")
#' mod <- glm(Temp ~ Wind, Solar.R, data = airquality, family = poisson())
#' test <- airquality[1:10, ]
#' .get_estimators(list(model), list(test))
#' @noRd
.get_estimators <- function(model, test_data) {
  
  # take models from training data to compute deviance, pseudo R-squared
  # and the predictions of the test part of the data
  aic <- mapply(function(X, Y) {
    try(AIC(X, newdata = Y), silent = TRUE)
  }, X = model, Y = test_data[])
  
  aic <- as.numeric(aic)
  
  Deviance <- mapply(function(X, Y) {
    try(deviance(X, newdata = Y), silent = TRUE)
  }, X = model, Y = test_data[])
  
  Deviance <- as.numeric(Deviance)
  
  pR2 <- t(mapply(function(X, Y) {
    try(pseudoR2(X), silent = TRUE)
  }, X = model, Y = test_data[]))
  
  pR2 <- matrix(as.numeric(unlist(pR2)),
                ncol = 5,
                nrow = length(model),
                dimnames = list(1:length(model),
                                dimnames(pR2)[[2]]))
  
  # and the predictions
  preds <-  mapply(function(X, Y) {
    try(predict(X, newdata = Y), silent = TRUE)
  }, X = model, Y = test_data[])
  
  gof <- cbind(data.frame(AIC = aic, 
                          deviance = Deviance), 
               pR2)
  
  
  estimators <- list(goodness_of_fit = gof,
                     predictions = preds)
  
  
  return(estimators)
  
}

#' @method print gosset_cv
#' @export
print.gosset_cv <- function(x, ...) {
  cat("Model formula:\n")
  cat(x[["raw"]][["call"]], "\n \n")
  cat("Cross-validation estimates: \n")
  print(round(unlist(x[["coeffs"]]), 4))
}

# Compute weighted means in cross-validation
.mean_crossvalidation <- function(object, folds = NULL, 
                                  mean.method = NULL, 
                                  ...){
  # take length of folds
  N <- length(folds)
  
  if (is.null(mean.method)) {
    mean.method <- "stouffer"
  }
  
  # Z-test weight mean
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
    m <- sum(stouffer, na.rm = TRUE)
  }
  
  # mean weighted by foldsize
  if (mean.method == "foldsize") {
    # make a table of folds and take
    # the number of observations per fold
    foldsize <- as.vector(table(folds))
    
    # fold size mean is the product of multiplication of object values by 
    # its number of observations then divided by the total number of observations
    m <- sum(object * foldsize, na.rm = TRUE) / sum(foldsize)
  }
  
  # arithmetic mean 
  if (mean.method == "equal") {
    m <- mean(object, na.rm = TRUE)
  }
  
  return(m)
  
}
