#' Forward stepwise regression for model selection
#'
#' Forward selection is a type of stepwise regression which begins with an empty 
#' model and adds in variables one by one. In each step one variable is added
#' when it makes a single best improvement to the model.
#'
#' @param formula a symbolic description of the model, if set as   
#'   \eqn{ y ~ . }   all variables in data are used  
#' @param select.by a character for the goodness-of-fit statistical parameter to
#' select the models. Set as 'deviance' by default.
#' @param ncores a integer for the number of cores to be used in the parallel computing. 
#' Set as 1 by default (no parallelisation)
#' @param packages an optional character vector of packages that the parallel tasks depend on, ignore
#' if all required packages are loaded to .GlobalEnv 
#' @inheritParams crossvalidation
#' @return The cross-validation goodness-of-fit estimates for the best model, which are:
#' \item{AIC}{Akaike Information Criterion}
#' \item{deviance}{Model deviance}
#' \item{logLik}{Log-Likelihood}
#' \item{MaxLik}{Maximum likelihood pseudo R-squared}
#' \item{CraggUhler}{Cragg and Uhler's pseudo R-squared}
#' \item{Agresti}{Agresti pseudo R-squared}
#' Cross-validation estimates are computed using the fitted models on the validation samples.
#' @seealso \code{\link{crossvalidation}}
#' @examples 
#' \dontrun{
#' 
#' library("gnm")
#' library("doParallel")
#' library("abind")
#' 
#' data("airquality")
#' 
#' mod <- forward(Temp ~ .,
#'                data = airquality,
#'                k = 3,
#'                select.by = "AIC",
#'                family = poisson(link = "log"))
#' 
#' }
#' @import doParallel
#' @import foreach
#' @import abind
#' @export
forward <- function(formula, data, k = NULL, folds = NULL, 
                    select.by = NULL, ncores = NULL, packages = NULL, ...) {
  
  n <- nrow(data)
  
  # check/define folds before the forward begans 
  # so all steps will use the same sample
  if(is.null(k)) {
    k <- 10
  }
  
  if (is.null(folds)) {
    folds <- sample(rep(1:k, times = ceiling(n/k), length.out = n),
                    replace = FALSE)
  }
  
  if (is.null(select.by)) {
    select.by <- "deviance"
  }
  
  opt.select <- c("AIC","deviance", "MaxLik", "CraggUhler")
  
  if(!select.by %in% opt.select) {
    stop("invalid method in select.by. Options are: ", 
         toString(opt.select), "\n")
  }
  
  if (is.null(ncores)) {
    ncores <- 1
  }
  
  # define initial parameters for forward selection
  # baseline 
  # if AIC or deviance, take a very high number
  if (select.by %in% c("AIC","deviance")) {
    baseline <-  1e+11
  } else {
    # if other method, take 0 as baseline
    baseline <- 0
  }
  
  # vector to keep best explanatory variables
  var_keep <- character()
  # keep running if TRUE
  best <- TRUE
  # number of runs
  counter <- 1
  # the names of explanatory variables and response
  exp_var <- c("empty_model", all.vars(formula)[-1])
  
  Y <- all.vars(formula)[1]
  
  if ("." %in% exp_var) {
    exp_var <- c("empty_model", names(data)[-match(Y, names(data))])
  } else {
    exp_var <- c("empty_model", all.vars(formula)[-1])
  }
  
  # add a empty variable to the model 
  data$empty_model <- rep(1, times = n)
  
  # a list to keep the parameters from each step 
  coeffs <- list()
  
  cat("\nCreating", ncores, "parallel cluster(s) of", parallel::detectCores(), 
      "cores. This may reduce the performance of your computer \n")
  
  # create cluster to do parallelisation
  cluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cluster)
  
  # keep running until the model get its best performance
  while (best) {
    
    cat("\nForward Selection. Step ", counter, "\n Time: ", date(), "\n")
    
    fs <- length(exp_var)
    
    dots <- list(...)
    
    args <- list(data = data, k = k, folds = folds)
    
    args <- c(args, dots)
    
    i <- 1:fs
    
    # get predictions from nodes and put in matrix
    models <- try(foreach::foreach(i = i,
                                   .combine = .comb,
                                   .packages = packages) %dopar% (.forward_dopar(as.formula(
                                     paste0(Y, " ~ ", paste(c(var_keep, exp_var[i]), collapse = " + "))
                                   ),
                                   args)))
    
    models <- t(models)
    
    # take the model with best parameter
    modpar <- models[, dimnames(models)[[2]] %in% select.by]
    
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
    
    # refresh baseline 
    baseline <- value_best
    
    # take the name of best variable
    best_model <- exp_var[index_best]
    
    if (best_model == "empty_model") { 
      best <- FALSE 
    }
    
    # model calls to add into list of parameters
    call_m <- paste0(Y, " ~ ", paste(paste(var_keep, collapse = " "), exp_var))
    call_m <- tibble::as_tibble(cbind(call = call_m, models))
    call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
    
    # take outputs from this run and add it to the list of parameters
    coeffs[[counter]] <- call_m
    
    if (best) {
      
      # remove best variable for the next run
      exp_var <- exp_var[!grepl(best_model, exp_var)]
      
      # remove empty var from the first run, no longer necessary
      exp_var <- exp_var[!grepl("empty_model", exp_var)]
      
      # keep this model for the next run
      var_keep <- c(var_keep, best_model)
      
      cat("   Best model found:", paste0(Y, " ~ ", paste(var_keep, collapse = " + ")) , "\n\n")
      
    }
    
    # update counter (number of runs in 'while')
    counter <- counter + 1
    
    # prevent while loop to broke when the model fits with all variables
    if(length(exp_var) == 0) {
      best <- FALSE
    }
    
  }
  
  # Stop cluster connection
  parallel::stopCluster(cluster)
  
  # Run a cross-validation with this model and take the outputs
  fform <- as.formula(paste0(Y, " ~ ", paste(c(var_keep), collapse = " + ")))
  model <- crossvalidation(fform,
                           data = data, 
                           k = k,
                           folds = folds,
                           ...)
  
  forwardraw <- list(selected_by = select.by,
                     forward_coeffs = coeffs)
  
  cat("\n\nEnd of forward selection.\n")
  
  # combine the raw data from forward regression with the cross-validation
  result <- c(model, forwardraw)
  
  class(result) <- c("crossvalidation", class(result))
  
  return(result)
}


# combine results from parallel
.comb <- function(...) {
  abind::abind(..., along = 2, force.array = TRUE)
}

# model call for parallel
.forward_dopar <- function(formula, args){
  
  args <- c(formula, args)
  
  m <- do.call(gosset::crossvalidation, args)
  
  result <- m[[1]]
  
  return(t(result))
  
}


# logical function for > greater 
.is_greater <- function(x, y) {
  x > y
}


# logical function for < lower
.is_lower <- function(x, y) {
  x < y
}

