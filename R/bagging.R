#' Bagging
#'
#' Methods for bagging
#'
#' @author Kauê de Sousa, Jacob van Etten and David Brown
#' @family model selection functions
#' @param formula an object of class "formula" (or one that can be 
#' coerced to that class): a symbolic description of the model to be fitted,
#' of the form \eqn{y ~ x1 + ... + xn}
#' @param data a data frame (or object coercible by as.data.frame to a data frame)
#' containing the variables in the model
#' @param N an integer for the number of observations in each bag
#' @param K an integer for the number of models in the bagging
#' \pkg{gosset} will set the seed randomly
#' @param ... additional arguments passed the methods of the chosen model
#' @return an object of class \code{gosset_cv} with the cross-validation 
#' @examples
#' @export
bagging <- function(formula, 
                    data, 
                    K = 100,
                    N = 1000,
                    ...)
{
  
  # get dots for extra arguments
  dots <- list(...)
  
  # create folds if needed, check length if given
  n <- nrow(data)
  
  # check if a seed is provided
  if (is.null(seed)) {
    seed <- as.integer(stats::runif(K, 0, 1000000))
  }
  
  namerows <- as.integer(rownames(data))
  
  folds <- list()
  
  # make the folds
  for (i in seq_along(seed)) {
    set.seed(seed[i])
    folds[[i]] <- sample(namerows, size = N, replace = TRUE)
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
  
  # fit the models
  mod <- lapply(folds, function(X) {
    
    traindata <- data[X, ]
    
    args <- list(formula = formula, data = traindata)
    
    args <- c(args, dots)
    
    do.call(model, args)
    
  })
  
  ncolpreds <- ncol(predict(mod[[1]], vcov = FALSE))
  namespreds <- dimnames(predict(mod[[1]], vcov = FALSE))[[2]]
  
  if (is.null(ncolpreds)) ncolpreds <- 1
  if (is.null(namespreds)) namespreds <- "preds"
  
  preds <- array(NA, 
                 dim = c(n, ncolpreds, K), 
                 dimnames = list(1:n, namespreds, 1:K))
  
  for (i in seq_along(mod)) {
    
    f <- unique(folds[[i]])
    
    testdata <- data[-f, ]
    
    p <- predict(mod[[i]], newdata = testdata, vcov = FALSE)
    
    preds[dimnames(p)[[1]], ,] <- p
    
  }
  
  result <- list(predictions = preds,
                 models = mod,
                 folds = folds, 
                 seeds = seeds)
                 
  return(result)
  
}



