#' Re-sample model estimates 
#' 
#' 
#' Applies a k-fold approach to re-sample 
#' estimates from PlackettLuce model. The function will 
#' subset the data into 'k' number folds and re-calculate
#' the model estimates. Optionally, a Bayesian bootstrapping 
#' technique can be used to increase output size and normalize 
#' the distribution of estimates 
#'
#' @param object a PlackettLuce model object
#' @param k an integer for the number of bins to subset the data
#' @param seed integer, the seed for random number generation. If NULL (the default),
#'  gosset will set the seed randomly
#' @param bootstrap logical, to run a Bayesian bootstrapping on object
#' @param ... additional arguments passed to methods, see details
#' @author Kauê de Sousa
#' @details
#' Additional details for Bayesian bootstrapping: 
#' \code{statistic} A function that accepts data as its first argument and possibly, 
#' the weights as its second, if use_weights is TRUE; \code{n1} The size of 
#' the bootstrap sample; \code{n2} The sample size used to calculate 
#' the statistic each bootstrap draw
#' 
#' @return A data frame with re-sampled estimates
#' @examples
#' library("PlackettLuce")
#' 
#' data("breadwheat", package = "gosset")
#' 
#' G = rank_tricot(breadwheat,
#'                 items = c("variety_a","variety_b","variety_c"),
#'                 input = c("overall_best","overall_worst"),
#'                 group = FALSE)
#' 
#' mod = PlackettLuce(G)
#' 
#' # default method, no bootstrapping and 5 folds
#' resample(mod)
#' 
#' resample(mod, log = FALSE)
#' 
#' # the argument 'seed' will make sure that the function 
#' # always return the same results
#' resample(mod, log = FALSE, seed = 1526)
#' 
#' # add bootstrapping
#' resample(mod, bootstrap = TRUE, log = FALSE, n1 = 100)
#' 
#' @importFrom PlackettLuce PlackettLuce
#' @importFrom stats coef
#' @export
resample = function(object, k = 5, bootstrap = FALSE, seed = NULL, ...){
  
  if(is.null(seed)) {
    seed = as.integer(runif(1, 1, 100000))
  }
  
  R = object$rankings
  
  n = nrow(R)
  
  set.seed(seed)
  folds = sample(rep(1:k, times = ceiling(n/k), length.out = n))
  
  e = data.frame()
  
  for(i in seq_len(k)) {
    m = PlackettLuce::PlackettLuce(R[folds != i, ])
    m = stats::coef(m, ...)
    m = data.frame(item = names(m),
                   estimate = as.numeric(m))
    e = rbind(e, m)
  }
  
  if (isTRUE(bootstrap)) {
    # split the data frame by item name and run the boostrapping
    e = split(e, e$item)
    
    e = lapply(e, function(x){
      z = bayes_boot(x$estimate, mean, ...)
      data.frame(item = x$item[1],
                 estimate = z)
    })
    
    e = do.call(rbind, e)
    
  }
  
  rownames(e) = 1:nrow(e)
  
  e = e[c("item", "estimate")]
  
  class(e) = union("gosset_df", class(e))
  
  return(e)
  
}

