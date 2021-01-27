#' Forward stepwise regression for model selection
#' 
#' Forward selection is a type of stepwise regression which begins with an empty
#'  model and adds in variables one by one. In each step one variable is added
#'  when it makes a single best improvement to the model.
#'
#' @author Kauê de Sousa and Jacob van Etten
#' @family model selection functions
#' @param formula a symbolic description of the model, if set as   
#'   \eqn{ y ~ . } all variables in \code{data} are used  
#' @param select.by a character for the goodness-of-fit statistical parameter to
#' select the models. Set as 'deviance' by default. Check 'Value' for details in outputs 
#' and goodness-of-fit options
#' @param ncores an integer for the number of cores to be used in the 
#'  parallel computing. Set as 1 by default (no parallelisation)
#' @param packages character vector of packages that the parallel tasks depend on. If you use 
#' GLM or GNM you should call for \pkg{gnm}, for Plackett-Luce, use \pkg{PlackettLuce}, for 
#' Bradley-Terry, use \pkg{psychotree}
#' @param akaike.weights optional, logical object for averaging the goodness-of-fit 
#'  coefficients with Akaike weights
#' @inheritParams crossvalidation
#' @return an object of class \code{gosset_cv} with the cross-validation 
#' goodness-of-fit estimates, which are:
#' \item{AIC}{Akaike Information Criterion}
#' \item{deviance}{Model deviance}
#' \item{logLik}{Log-Likelihood}
#' \item{MaxLik}{Maximum likelihood pseudo R-squared}
#' \item{CraggUhler}{Cragg and Uhler's pseudo R-squared}
#' \item{Agresti}{Agresti pseudo R-squared}
#' Cross-validation estimates are computed using the fitted models on the validation samples.
#' @examples 
#'  
#' 
#' require("gnm")
#' require("foreach")
#' require("abind")
#' require("doParallel")
#' 
#'  
#' data("airquality")
#' 
#' mod <- forward(Temp ~ .,
#'                data = airquality,
#'                k = 3,
#'                select.by = "AIC",
#'                packages = "gnm",
#'                family = poisson(link = "log"))
#'                
#' \donttest{
#' ##########################################
#' # Bradley-Terry Model
#' require("psychotree")
#' 
#' # Germany's Next Topmodel 2007 data from psychotree
#' data("Topmodel2007", package = "psychotree")
#' 
#' forward(preference ~ .,
#'         data = Topmodel2007,
#'         k = 5,
#'         packages = "psychotree",
#'         alpha = 0.01,
#'         minsize = 30)
#'   
#' }
#' @importFrom abind abind
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster
#' @importFrom stats as.formula
#' @export
forward <- function(formula, data, k = NULL, folds = NULL,
                    select.by = "deviance", akaike.weights = FALSE,
                    ncores = 1, packages = NULL, seed = NULL, ...) {

  # list of additional arguments
  dots <- list(...)
  
  # nrow in data
  n <- dim(data)[[1]]

  # assign folds
  if (is.null(folds)) {
    
    # check if a seed is provided
    if (is.null(seed)) {
      seed <- as.integer(stats::runif(1, 0, 10000))
    }
    
    if (is.null(k)) {
      stop("\nargument 'k' is missing with no default\n")
    }
    
    set.seed(seed)
    
    folds <- sample(rep(1:k, times = ceiling(n / k), length.out = n))
    
  }
  
  if (is.list(folds)) {
    k <- length(folds)
  }

  opt.select <- c("AIC","deviance","logLik",
                  "MaxLik","CraggUhler", "Agresti")

  if (!select.by %in% opt.select) {
    stop("invalid method in select.by. Options are: ",
         toString(opt.select), "\n")
  }

  # check if models must be selected by akaike.weights
  aw <- akaike.weights

  # Define initial parameters for forward selection
  # baseline if AIC or deviance without akaike.weights
  # take a very high number
  if (select.by %in% c("AIC","deviance") & isFALSE(aw)) {
    baseline <-  1e+11
  } else {
    # if other method, take 0 as baseline
    baseline <- 0
  }

  # vector to keep best explanatory variables
  var_keep <- character(0L)

  # keep running if TRUE
  best <- TRUE

  # number of runs
  counter <- 1
  
  # a list to keep the goodness-of-fit coefficients from each step
  coeffs <- list()

  # get the names of explanatory and response variables
  exp_var <- union("empty_model", all.vars(formula)[-1])

  Y <- all.vars(formula)[1]

  # if explanatory variables are not specified
  # take then from data
  if ("." %in% exp_var) {
    exp_var <- union("empty_model", names(data)[-match(Y, names(data))])
  } else {
    exp_var <- union("empty_model", all.vars(formula)[-1])
  }

  # add a empty variable to the model
  data$empty_model <- rep(0, times = n)
  # keep only the response variable and explanatory variables
  data <- data[, c(Y, exp_var)]

  message("\nCreating ", ncores, " parallel cluster(s) out of ", 
          parallel::detectCores(),
          " cores \n")

  # create cluster to do parallelisation
  cluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cluster)

  # keep running until the model get its best performance
  while (best) {

    message("\nForward Selection. Step ", counter, "\n Time: ", date(), "\n")

    fs <- length(exp_var)

    args <- list(data = data,
                 k = k,
                 folds = folds)

    args <- c(args, dots)

    i <- seq_len(fs)

    # get predictions from nodes and put in matrix
    models <- try(foreach::foreach(i = i,
                                   .combine = .comb,
                                   .packages = packages) %dopar% (.forward_dopar(
                                     stats::as.formula(paste0(Y, " ~ ", 
                                                       paste(c(var_keep, exp_var[i]),
                                                             collapse = " + "))), 
                                     args)
                                     )
                  )

    dimnames(models) <- list(seq_len(fs),
                             paste0("bin", seq_len(k)),
                             opt.select)

    # take the matrix with selected goodness of fit
    modpar <- models[, , dimnames(models)[[3]] %in% select.by]

    # this is to avoid matrix error when only one par is retained
    # for example when all variables are taken and this is the last 
    # round of while(best)
    if (is.null(dim(modpar))) {
      modpar <- t(as.matrix(modpar))
    }

    # if akaike.weights TRUE
    # then calculate it and take the highest value
    if (isTRUE(aw)) {

      # calculate akaike weigths
      # adjust function to the matrix arrangement
      if (nrow(modpar) > 1) {
        modpar <- apply(modpar, 2, function(x) {
          akaike_weights(x)[["akaike_weights"]]
        })
      } else {
        modpar <- apply(modpar, 1, function(x) {
          akaike_weights(x)[["akaike_weights"]]
        })
        # turn it into a matrix again
        modpar <- t(as.matrix(modpar))
      }

      # then take the stouffer mean
      modpar <- apply(modpar, 1, function(x) {
        .mean_crossvalidation(object = x,
                              folds = folds,
                              mean.method = "stouffer")
        })

      index_best <- which.max(modpar)

      value_best <- modpar[index_best]

      best <- .is_greater(value_best, baseline)


    }

    # if models are not selected with Akaike weights E
    # select accordingly to the chosen method
    if (isFALSE(aw)) {

      modpar <- apply(modpar, 1, function(x){
        .mean_crossvalidation(x,
                              folds = folds, 
                              ...)
      })

      # if AIC or deviance are selected then the model
      # with lower value is the best
      # other methods take the higher value
      if (select.by %in% c("AIC","deviance")) {

        index_best <- which.min(modpar)

        value_best <- modpar[index_best]

        best <- .is_lower(value_best, baseline)

      } else {

        index_best <- which.max(modpar)

        value_best <- modpar[index_best]

        best <- .is_greater(value_best, baseline)
      }
    }

    # refresh baseline
    baseline <- value_best

    # take the name of best variable
    best_model <- exp_var[index_best]

    # this is to prevent error in the array dimension
    # if all variables are included in the model
    if (length(dim(models)) == 3) {
      models_avg <- apply(models, c(1,3), function(x){
        .mean_crossvalidation(x,
                              folds = folds)
      })

    }else{
      models_avg <- apply(models, 2, function(x){
        .mean_crossvalidation(x,
                              folds = folds)
      })
    }

    # model calls to add into list of parameters
    call_m <- paste0(Y, " ~ ", paste(paste(var_keep, collapse = " "), exp_var))
    call_m <- tibble::as_tibble(cbind(call = call_m, models_avg))
    call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
    call_m <- list(call_m, models)

    # take outputs from this run and add it to the list of parameters
    coeffs[[counter]] <- call_m

    if (length(best_model) == 0) {
      warning("Error predicting one or more folds. Check your input data. \n")
      parallel::stopCluster(cluster)
      return(call_m)
    }

    if (best_model == "empty_model") {
      best <- FALSE
      var_keep <- "empty_model"

      message("No model identified to surpass the intercept-only model\n")

    }

    if (best) {

      # remove best variable for the next run
      exp_var <- exp_var[!grepl(best_model, exp_var)]

      # remove empty var from the first run, no longer necessary
      exp_var <- exp_var[!grepl("empty_model", exp_var)]

      # keep this model for the next run
      var_keep <- c(var_keep, best_model)

      message("Best model found: ",
          paste0(Y, " ~ ", paste(var_keep, collapse = " + ")),
          "\n\n")

    }

    # update counter (number of runs in 'while')
    counter <- counter + 1

    # prevent while loop to broke if the model fits with all variables
    if (length(exp_var) == 0) {
      best <- FALSE
      message("All variables in 'data' were selected \n")
    }

  }

  # Stop cluster connection
  parallel::stopCluster(cluster)

  # Run a cross-validation with this model and take the outputs
  fform <- as.formula(paste0(Y, " ~ ",
                             paste(c(var_keep), collapse = " + ")))

  args <- list(formula = fform,
               data = data,
               k,
               folds)

  args <- c(args, dots)

  model <- do.call("crossvalidation", args)

  forward_raw <- list(selected_by = select.by,
                      forward_coeffs = coeffs)

  message("\n\nEnd of forward selection.\n")

  # combine the raw data from forward regression with the cross-validation
  result <- c(model, forward_raw)

  class(result) <- union("gosset_cv", class(result))

  return(result)
}


# combine results from parallel
.comb <- function(...) {
  abind::abind(..., along = 1, force.array = TRUE)
}

# model call for parallel
.forward_dopar <- function(formula, args){

  args <- c(formula, args)

  m <- do.call(gosset::crossvalidation, args)

  result <- m$raw$estimators
  
  keepthis <- c("AIC","deviance","logLik", "MaxLik","CraggUhler", "Agresti")
  
  result <- result[, keepthis]
  
  nfold <- m$raw$k

  result  <- array(unlist(result), c(1, nfold, 6))

  return(result)

}

