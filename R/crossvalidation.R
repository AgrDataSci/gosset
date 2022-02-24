#' Cross-validation 
#'
#' Methods for measuring the performance of a predictive model on sets of 
#' test data in Bradley-Terry model from \pkg{psychotree}, Generalized Linear and 
#' Generalized Nonlinear models from \pkg{gnm}, and Plackett-Luce model from 
#' \pkg{PlackettLuce}
#'
#' @author Kauê de Sousa, Jacob van Etten and David Brown
#' @family model selection functions
#' @param formula an object of class "formula" (or one that can be 
#' coerced to that class): a symbolic description of the model to be fitted,
#' of the form \eqn{y ~ x1 + ... + xn}
#' @param data a data frame (or object coercible by as.data.frame to a data frame)
#' containing the variables in the model
#' @param k an integer for the number of bins in the cross-validation
#' @param folds an optional vector or list of vectors specifying the \var{k}-folds 
#' in the cross-validation
#' @param mean.method a character for the method to calculate the mean of 
#' cross-validation estimators. 
#' Options are: 'equal', arithmetic mean; 
#' 'foldsize', weighted mean by the size in each fold; 
#' 'Ztest' weighted through Z-test. See references 
#' @param logLik.method a character for the method to compute logLik,
#'  options are "tree" (default) or "worth"
#' @param seed integer, the seed for random number generation. If \code{NULL} (the default), 
#' \pkg{gosset} will set the seed randomly
#' @param ... additional arguments passed the methods of the chosen model
#' @return an object of class \code{gosset_cv} with the cross-validation 
#' goodness-of-fit estimates, which are:
#' \item{AIC}{Akaike Information Criterion}
#' \item{deviance}{Model deviance}
#' \item{logLik}{Log-Likelihood}
#' \item{MaxLik}{Maximum likelihood pseudo R-squared}
#' \item{CraggUhler}{Cragg and Uhler's pseudo R-squared}
#' \item{McFadden}{McFadden pseudo R-squared}
#' \item{kendallTau}{the Kendall correlation coefficient, only for Plackett-Luce models}
#' @seealso \code{\link[psychotree]{bttree}}, 
#' \code{\link[gnm]{gnm}},
#' \code{\link[PlackettLuce]{pltree}}
#' @references 
#' 
#' Elder J. F. (2003). Journal of Computational and Graphical Statistics, 12(4), 853–864.
#' \doi{https://doi.org/10.1198/1061860032733}
#' 
#' James G., et al. (2013). An Introduction to Statistical Learning: with Applications in R.
#' \doi{https://doi.org/10.1007/978-1-4614-7138-7}
#' 
#' Whitlock M. C. (2005). Journal of Evolutionary Biology, 18(5), 1368–1373. 
#' \doi{https://doi.org/10.1111/j.1420-9101.2005.00917.x}
#' 
#' @examples
#' 
#' # Generalized Linear Models
#' if (require("gnm")) {
#' data("airquality")
#' 
#' cv <- crossvalidation(Temp ~ Wind + Solar.R,
#'                       data = airquality,
#'                       k = 3,
#'                       seed = 999,
#'                       family = poisson())
#' }
#' 
#' \donttest{
#' # Folds as list 
#' if(require("caret")) {
#' 
#' fd <- createFolds(y = airquality$Day, k = 15, list = TRUE, returnTrain = TRUE)
#'  
#' crossvalidation(Temp ~ .,
#'                 data = airquality,
#'                 folds = fd,
#'                 family = poisson(link = "log"))
#' }
#' ########################################
#'   
#' # Plackett-Luce Model
#' if(require("PlackettLuce")) {
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
#' }
#' }
#'                 
#' @importFrom stats model.frame model.response runif
#' @export
crossvalidation <- function(formula,
                            data, 
                            k = 10,
                            folds = NULL, 
                            mean.method = "Ztest",
                            logLik.method = "tree",
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
      seed <- as.integer(stats::runif(1, 0, 1000000))
    }
    
    set.seed(seed)
    
    folds <- sample(rep(1:k, times = ceiling(n / k), length.out = n))
    
  }
  
  # validate mean.method
  mean.opt <- c("Ztest", "foldsize", "equal")
  
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
  # folds as numeric vector - gosset style
  if (!is.list(folds)) {
  train <- list()
  for (i in 1:k) {
    train[[i]] <- data[folds != i ,]
  }
  
  test <- list()
  for (i in 1:k) {
    test[[i]] <- data[folds == i  ,]
    }
  }
  
  #folds as list - caret style
  if (is.list(folds)) {
    
    k <- length(folds)
    mean.method <- "equal"
    message("\nmean.method set to 'equal' as folds is a list \n")
    
    train <- list()
    for (i in 1:k) {
      train[[i]] <- data[folds[[i]], ]
    }
    
    test <- list()
    for (i in 1:k) {
      test[[i]] <- data[-folds[[i]], ]
    }
  }
  
  # fit the models
  mod <- lapply(train, function(X) {
    args <- list(formula = formula, data = X)
    args <- c(args, dots)
    try(do.call(model, args))
    
  })
  
  # get goodness-of-fit estimates from models
  # take models from training data to compute deviance, pseudo R-squared
  # and the predictions of the test part of the data
  estimators <- try(mapply(function(X, Y) {
    a <- AIC(X, newdata = Y)
    d <- deviance(X, newdata = Y, method = logLik.method)
    p <- pseudoR2(X, newdata = Y, method = logLik.method)
    data.frame(AIC = a, 
               deviance = d, p)
  }, X = mod, Y = test[]), silent = FALSE)
  
  if ("try-error" %in% class(estimators)) {
    estimators <- matrix(0, nrow = k, ncol = 7, byrow = TRUE)
  }
  
  nms <- dimnames(estimators)[[1]]
  
  estimators <- matrix(unlist(estimators), nrow = k, ncol = 7, byrow = TRUE)
  
  dimnames(estimators)[[2]] <- nms
  
  # and the predictions
  preds <-  mapply(function(X, Y) {
    try(predict(X, newdata = Y), silent = TRUE)
  }, X = model, Y = test[])
  
  # if model is pltree take the kendall cor 
  if (model == "pltree") {
    
    # get the original rankings from each bin in test
    R_pl <- all.vars(formula)[[1]]
    
    R_pl <- lapply(train, function(x) {
      
      x <- x[, R_pl]
      
      x[1:length(x), , as.grouped_rankings = FALSE]
      
    })
    
    preds <- lapply(mod, function(x) {
      predict(x)
    })
    
    # when additional rankings are added to place the local item 
    # we should rescale the matrix in predictions
    rescale <- nrow(R_pl[[1]]) / nrow(preds[[1]]) == 4
    
    if (rescale) {
      
      preds <- lapply(preds, function(x) {
        
        x <- rbind(x, x, x, x)
        
      })
      
    }
    
    KT <- mapply(function(X, Y) {
      
      try(kendallTau(X, Y)[[1]], silent = TRUE)
      
    }, X = R_pl, Y = preds)
    
    KT <- as.numeric(KT)
    
    estimators <- cbind(estimators, kendallTau = KT)
      
  }
  
  # estimators are then averaged weighted by 
  # number of predicted cases using selected mean method
  # Z-test weight mean
  N <- dim(data)[1]
  
  if (is.list(folds)) {
    mean.method <- "equal" 
  }
  
  if (mean.method == "Ztest") {
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
    
    # then we multiply the input values by the
    # weight of each fold
    # sum these values and that is the Ztest mean
    means <- apply(estimators, 2, function(x){
      m <- x * wfold
      sum(m, na.rm = TRUE)
    })
  }
  
  # mean weighted by foldsize
  if (mean.method == "foldsize") {
    # make a table of folds and take
    # the number of observations per fold
    foldsize <- as.vector(table(folds))
    
    # fold size mean is the product of multiplication of object values by 
    # its number of observations then divided by the total number of observations
    means <- apply(estimators, 2, function(x){
      sum(x * foldsize, na.rm = TRUE) / sum(foldsize)
    })
  }
  
  # arithmetic mean 
  if (mean.method == "equal") {
    means <- apply(estimators, 2, function(x){
      mean(x, na.rm = TRUE) 
    })
  }
  
  # means and estimates as data frame
  means <- as.data.frame(t(means))
  
  names(means) <- dimnames(estimators)[[2]]
   
  class(means) <- union("gosset_df", class(means))
  
  estimators <- as.data.frame(estimators)

  class(estimators) <- union("gosset_df", class(estimators))
  
  result <- list(coeffs = means,
                 raw = list(call = deparse(formula, width.cutoff = 500),
                            estimators = estimators,
                            k = k,
                            folds = folds,
                            models = mod,
                            data = data))
  
  class(result) <- union("gosset_cv", class(result))
  
  return(result)
  
}


#' @method print gosset_cv
#' @export
print.gosset_cv <- function(x, ...) {
  cat("Model formula:\n")
  cat(x[["raw"]][["call"]], "\n \n")
  cat("Weighted Cross-validation estimates: \n")
  print(x$coeffs)
}
