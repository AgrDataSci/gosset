#' Cross-validation 
#'
#' Methods for measuring the performance of a predictive model on sets of 
#' test data in Bradley-Terry model from \pkg{psychotree}, Generalized Linear and 
#' Generalized Nonlinear models from \pkg{gnm}, and Plackett-Luce model from 
#' \pkg{PlackettLuce}. The package that provides the model (\pkg{gnm},
#' \pkg{PlackettLuce} or \pkg{psychotree}) must be attached with
#' \code{library()} before calling \code{crossvalidation()}.
#'
#' @author Kauê de Sousa, Jacob van Etten and David Brown
#' @family model selection functions
#' @param formula an object of class "formula" (or one that can be 
#' coerced to that class): a symbolic description of the model to be fitted,
#' of the form \eqn{y ~ x1 + ... + xn}
#' @param data a data frame (or object coercible by as.data.frame to a data frame)
#' containing the variables in the model
#' @param k an integer for the number of bins in the cross-validation
#' @param folds an optional vector or list specifying the \var{k}-folds in
#' the cross-validation. A vector gives, for each row of \code{data}, the
#' fold (1 to \var{k}) in which that row is held out for testing. A list
#' gives, for each fold, the row positions of \code{data} used for
#' training, as returned by \code{caret::createFolds(..., returnTrain = TRUE)}.
#' Rows dropped for missing values are removed from the folds
#' @param seed integer, the seed used to create the folds. If \code{NULL}
#' (the default), folds are drawn from the current random number stream.
#' When a seed is given, the caller's random number state is restored on exit
#' @param ... additional arguments passed to the methods of the chosen model
#' @param object a model object
#' @param newdata a data.frame with test data
#' @return an object of class \code{gosset_cv} with the cross-validation 
#' goodness-of-fit estimates, which are:
#' \item{AIC}{Akaike Information Criterion}
#' \item{deviance}{Model deviance}
#' \item{logLik}{Log-Likelihood}
#' \item{MaxLik}{Maximum likelihood pseudo R-squared}
#' \item{CraggUhler}{Cragg and Uhler's pseudo R-squared}
#' \item{McFadden}{McFadden pseudo R-squared}
#' \item{kendallTau}{the Kendall correlation coefficient}
#' @seealso \code{\link[psychotree]{bttree}}, 
#' \code{\link[gnm]{gnm}},
#' \code{\link[PlackettLuce]{pltree}}
#' @references 
#' 
#' Elder J. F. (2003). Journal of Computational and Graphical Statistics, 
#' 12(4), 853–864. \doi{https://doi.org/10.1198/1061860032733}
#' 
#' James G., et al. (2013). \doi{https://doi.org/10.1007/978-1-4614-7138-7}
#' 
#' Whitlock M. C. (2005). Journal of Evolutionary Biology, 18(5), 1368–1373. 
#' \doi{https://doi.org/10.1111/j.1420-9101.2005.00917.x}
#' 
#' @examples 
#' \donttest{
#' # Generalized Linear Models
#' if (require("gnm")) {
#' data("airquality")
#' 
#' cv = crossvalidation(Temp ~ Wind + Solar.R,
#'                       data = airquality,
#'                       k = 3,
#'                       seed = 999,
#'                       family = poisson())
#' }                    
#' # Plackett-Luce Model
#' if(require("PlackettLuce")) {
#' # beans data from PlackettLuce
#' data("beans", package = "PlackettLuce")
#' 
#' G = rank_tricot(data = beans,
#'                  items = c(1:3),
#'                  input = c(4:5),
#'                  additional.rank = beans[c(6:8)],
#'                  group = TRUE)
#' 
#' beans = cbind(G, beans)
#' 
#' # take seasons as bins
#' k = length(unique(beans$season))
#' folds = as.integer(as.factor(beans$season))
#' 
#' cv = crossvalidation(G ~ maxTN,
#'                       data = beans,
#'                       k = k,
#'                       folds = folds,
#'                       minsize = 100)
#' }
#' }
#'                 
#' @importFrom stats model.frame model.response runif
#' @export
crossvalidation = function(formula,
                           data, 
                           k = 10,
                           folds = NULL, 
                           seed = NULL,
                           ...)
{
  
  # get dots for extra arguments
  dots = list(...)
  
  n_input = nrow(data)
  
  # create the model frame first, so that rows dropped for missing
  # values are known before folds are created or aligned
  environment(formula) = parent.frame()
  
  data = stats::model.frame(formula, data)
  
  # positions of the input rows kept in the model frame
  dropped = as.integer(attr(data, "na.action"))
  keep = setdiff(seq_len(n_input), dropped)
  n = nrow(data)
  
  if (is.list(folds)) k = length(folds)
  if (length(k) != 1L || is.na(k) || !is.numeric(k) ||
      k != floor(k) || k < 2L || k > n) {
    stop('k must be an integer between 2 and the number of complete observations')
  }
  
  # assign folds
  if (is.null(folds)) {
    
    if (is.null(seed)) {
      # no seed: draw from the current random stream, as any R function would
      folds = sample(rep(1:k, times = ceiling(n / k), length.out = n))
    } else {
      # seed given: reproducible folds, without changing the caller's
      # random number stream
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        old_seed = get(".Random.seed", envir = globalenv(), inherits = FALSE)
        on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
      } else {
        on.exit(if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
          rm(".Random.seed", envir = globalenv())
        }, add = TRUE)
      }
      set.seed(seed)
      folds = sample(rep(1:k, times = ceiling(n / k), length.out = n))
    }
    
  } else if (!is.list(folds)) {
    
    # folds supplied as a vector refer to the rows of the input data
    if (length(folds) != n_input || anyNA(folds) ||
        !setequal(unique(folds), seq_len(k))) {
      stop('folds must assign every row of data to one of the k folds, numbered 1 to k')
    }
    folds = folds[keep]
    
  } else {
    
    # folds supplied as a list hold row positions of the input data;
    # map them to the rows kept in the model frame
    bad = vapply(folds, function(f) {
      !is.numeric(f) || anyNA(f) || any(f < 1 | f > n_input | f != floor(f))
    }, logical(1))
    if (any(bad)) {
      stop('each element of folds must hold row positions of data')
    }
    folds = lapply(folds, function(f) {
      f = match(f, keep)
      f[!is.na(f)]
    })
    
  }
  
  Y = stats::model.response(data)
  
  # define the most suitable model based on the 
  # class of the response variable
  model = "gnm"
  
  if (.is_grouped_rankings(Y)) {
    
    model = "pltree"
    
    message("Using Plackett-Luce model\n")
    
  }
  
  if (.is_paircomp(Y)) {
    
    model = "bttree"
    
    message("Using Bradley-Terry model\n")
    
  }
  
  # split data into lists with training and test set
  # folds as numeric vector - gosset style
  if (!is.list(folds)) {
    train = list()
    for (i in 1:k) {
      train[[i]] = data[folds != i ,]
    }
    
    test = list()
    for (i in 1:k) {
      test[[i]] = data[folds == i  ,]
    }
  }
  
  # folds as list - caret style: each element holds the rows used for
  # TRAINING in that fold (as returned by caret::createFolds with
  # returnTrain = TRUE); the remaining rows form the test set
  if (is.list(folds)) {
    
    k = length(folds)
    
    train = list()
    for (i in 1:k) {
      train[[i]] = data[folds[[i]], ]
    }
    
    test = list()
    for (i in 1:k) {
      test[[i]] = data[-folds[[i]], ]
    }
  }
  
  if (any(vapply(train, nrow, integer(1)) == 0L) ||
      any(vapply(test, nrow, integer(1)) == 0L)) {
    stop('All cross-validation training and test folds must be non-empty')
  }

  # resolve the model function from its package, so that the fit does not
  # depend on the package being attached by the user
  pkg = switch(model, gnm = "gnm", pltree = "PlackettLuce", bttree = "psychotree")
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("package '", pkg, "' is required for this model; please install it")
  }
  # the package must also be attached: pltree() evaluates plfit() in the
  # caller's environment, and the null models in pseudoR2() are refitted
  # with update(), which calls the model function by name
  if (!paste0("package:", pkg) %in% search()) {
    stop("crossvalidation() needs package '", pkg, "' to be attached. ",
         "Run library(", pkg, ") first")
  }
  model_fun = getExportedValue(pkg, model)
  
  # fit the models
  mod = lapply(train, function(X) {
    args = list(formula = formula, data = X)
    args = c(args, dots)
    try(do.call(model_fun, args))
    
  })
  
  failed = vapply(mod, inherits, logical(1), what = 'try-error')
  if (any(failed)) {
    stop('Model fitting failed in fold(s): ', paste(which(failed), collapse = ', '))
  }

  # get goodness-of-fit estimates from models
  # take models from training data to compute deviance, pseudo R-squared
  # and the predictions of the test part of the data
  estimators = try(mapply(function(X, Y) {
    a = AIC(X, newdata = Y)
    d = deviance(X, newdata = Y)
    p = pseudoR2(X, newdata = Y)
    data.frame(AIC = a, 
               deviance = d, p)
  }, X = mod, Y = test[]), silent = FALSE)
  
  if (inherits(estimators, 'try-error')) {
    stop('Cross-validation estimator calculation failed; no scores returned')
  }
  
  nms = dimnames(estimators)[[1]]
  
  estimators = matrix(unlist(estimators), nrow = k, ncol = length(nms), byrow = TRUE)
  
  dimnames(estimators)[[2]] = nms
  
  # Predictions for pltree are handled below. Other model families
  # do not currently use predictions in the returned summary.
  # if model is pltree take the kendall correlation
  if (model == "pltree") {
    
    # observed rankings in each held-out test set
    R_pl = all.vars(formula)[[1]]
    
    R_pl = lapply(test, function(x) {
      
      x = x[, R_pl]
      
      x[1:length(x), , as.grouped_rankings = FALSE]
      
    })
    
    # predictions for the held-out test set from the model fitted on the
    # training set, so that Kendall's tau is an out-of-sample measure
    preds = mapply(function(X, Y) {
      predict(X, newdata = Y)
    }, X = mod, Y = test, SIMPLIFY = FALSE)
    
    # when additional rankings are added to place the local item 
    # we should rescale the matrix in predictions
    rescale = nrow(R_pl[[1]]) / nrow(preds[[1]]) == 4
    
    if (rescale) {
      
      preds = lapply(preds, function(x) {
        
        x = rbind(x, x, x, x)
        
      })
      
    }
    
    KT = mapply(function(X, Y) {
      
      try(kendallTau(X, Y)[[1]], silent = TRUE)
      
    }, X = R_pl, Y = preds)
    
    KT = as.numeric(KT)
    
    estimators = cbind(estimators, kendallTau = KT)
    
  }
  
  # pseudo-R2 estimators are then averaged weighted by 
  # number of predicted cases using Z-test weight mean
  N = dim(data)[1]
  
  # number of observations in each test set; this works for folds
  # given as a vector or as a list
  foldsize = vapply(test, nrow, integer(1))
  
  # take the weight of each fold
  # first, the squared root of foldsize (observations per fold)
  # by the total number of observation
  wfold = sqrt(as.vector(foldsize) / N)
  
  # then divide this vector by its sum
  wfold = wfold / sum(wfold)
  
  # then we multiply the input values by the
  # weight of each fold
  # sum these values and that is the Ztest mean
  means = apply(estimators, 2, function(x){
    m = x * wfold
    sum(m, na.rm = TRUE)
  })
  
  # means and estimates as data frame
  means = as.data.frame(t(means))
  
  # AIC, deviance and logLik should be the sum of fold values
  means[1,1:4] = as.data.frame(t(colSums(estimators[, 1:4])))
  
  class(means) = union("gosset_df", class(means))
  
  estimators = as.data.frame(estimators)
  
  class(estimators) = union("gosset_df", class(estimators))
  
  result = list(coeffs = means,
                raw = list(call = deparse(formula, width.cutoff = 500),
                           estimators = estimators,
                           k = k,
                           folds = folds,
                           models = mod,
                           data = data))
  
  class(result) = union("gosset_cv", class(result))
  
  return(result)
  
}

#' @method print gosset_cv
#' @export
print.gosset_cv = function(x, ...) {
  cat("Model formula:\n")
  cat(x[["raw"]][["call"]], "\n \n")
  cat("Cross-validation estimates: \n")
  print(x$coeffs)
}

