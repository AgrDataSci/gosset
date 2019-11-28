#' Cross-validation 
#'
#' Methods for measuring the performance of a predictive model on sets of test data. 
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted,
#' of the form \eqn{ y ~ x1 + ... + xn }
#' @param data a data frame, list or environment 
#' (or object coercible by as.data.frame to a data frame) 
#' containing the variables in the model.
#' @param k an integer for the number of splits in the cross-validation
#' @param folds an optional vector specifying the folds in the cross-validation
#' @param mean.method a character for the method to calculate the mean of cross-validation estimators. 
#' Options are: 'equal', an arithmetic mean; 
#' 'foldsize', a weighted mean by the size in each fold; 
#' 'stouffer' a weighted Z-test. See references. 
#' @param seed The seed for random number generation. If NULL (the default), gosset will set the seed randomly
#' @param ... additional arguments passed to methods
#' @return The cross-validation goodness-of-fit estimates, which are:
#' \item{AIC}{Akaike Information Criterion}
#' \item{deviance}{Model deviance}
#' \item{logLik}{Log-Likelihood}
#' \item{MaxLik}{Maximum likelihood pseudo R-squared}
#' \item{CraggUhler}{Cragg and Uhler's pseudo R-squared}
#' \item{Agresti}{Agresti pseudo R-squared}
#' @seealso \code{\link[gnm]{gnm}}, \code{\link[PlackettLuce]{pltree}}, 
#' \code{\link[psychotree]{bttree}}
#' @references 
#' Agresti A. (2002). Categorical Data Analysis. John Wiley & Sons, Inc., Hoboken, NJ, USA. http://doi.wiley.com/10.1002/0471249688
#' 
#' Elder J. F. (2003). Journal of Computational and Graphical Statistics, 12(4), 853–864. https://doi.org/10.1198/1061860032733.
#' 
#' Whitlock M. C. (2005). Journal of Evolutionary Biology, 18(5), 1368–1373. https://doi.org/10.1111/j.1420-9101.2005.00917.x.
#' @examples
#' # Generalized Linear Models
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
#' # PlackettLuce Model
#' # beans data from PlackettLuce
#' library("PlackettLuce")
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
#' # BradleyTerry Model
#' library("psychotree")
#' # Germany's Next Topmodel 2007 data from psychotree
#' data("Topmodel2007", package = "psychotree")
#' 
#' cv <- crossvalidation(preference ~ .,
#'                       data = Topmodel2007,
#'                       k = 5)
#'                 
#' }
#'                 
#' @import partykit
#' @import psychotools
#' @import psychotree
#' @import PlackettLuce
#' @import gnm
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
      seed <- as.integer(runif(1, 0, 10000))
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
  
  # drop folds
  drop.folds <- dots[["drop.folds"]] 
  if(!is.null(drop.folds)) {
    cat("Folds ", paste(drop.folds, collapse = ", "),
        "are excluded from modelling exercise\n")
    train <- train[-drop.folds]
    test  <- test[-drop.folds]
    folds <- folds[!folds %in% drop.folds]
    k <- length(unique(folds))
    drop <- match("drop.folds", names(dots))
    dots <- dots[-drop]

  }
  
  # fit the models
  mod <- lapply(train, function(X) {
      
      args <- list(formula = formula, data = X)
      
      args <- c(args, dots)
      
      try(do.call(model, args))
      
    })
  
  # take models from training data to compute deviance, pseudo R-squared
  # and the predictions of the test part of the data
  aic <- mapply(function(X, Y) {
    try(AIC(X, newdata = Y), silent = TRUE)
  }, X = mod, Y = test[])
  
  aic <- as.numeric(aic)
  
  Deviance <- mapply(function(X, Y) {
    try(deviance(X, newdata = Y), silent = TRUE)
  }, X = mod, Y = test[])
  
  Deviance <- as.numeric(Deviance)
  
  logLik <- Deviance / -2
  
  pR2 <- t(mapply(function(X, Y) {
    try(pseudoR2(X, newdata = Y), silent = TRUE)
  }, X = mod, Y = test[]))
  
  pR2 <- matrix(unlist(pR2), 
                ncol = 5, 
                nrow = k, 
                dimnames = list(1:k, dimnames(pR2)[[2]]))
  
  # no need to keep null logLik
  pR2 <- pR2[, -c(1,2)]
  
  estimators <- cbind(data.frame(AIC = aic, 
                                 deviance = Deviance, 
                                 logLik = logLik),
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
  

  result <- list(coeffs = means,
                 raw = list(call = deparse(formula, width.cutoff = 500),
                            estimators = estimators,
                            folds = folds,
                            models = mod,
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
  
  if (is.null(mean.method)) {
    mean.method <- "stouffer"
    }
  
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
    mean <- sum(object * foldsize) / sum(foldsize)
  }
  
  if (mean.method == "equal") {
    mean <- mean(object)
  }
  
  return(mean)
  
}