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



#' Performs a Bayesian bootstrap 
#' 
#' Function obtained from Rasmus Bååth's blog at 
#' https://www.sumsar.net/blog/2015/07/easy-bayesian-bootstrap-in-r/
#' 
#' @author Rasmus Bååth
#' @param data The data as either a vector, matrix or data.frame
#' @param statistic A function that accepts data as its first argument and possibly, 
#'  the weights as its second, if use_weights is \code{TRUE}. Should return a numeric vector
#' @param n1 The size of the bootstrap sample
#' @param n2 The sample size used to calculate the statistic each bootstrap draw
#' @param use_weights  Whether the statistic function accepts a weight argument or
#'   should be calculated using resampled data
#' @param weight_arg   If the statistic function includes a named argument for the
#'   weights this could be specified here
#' @param ... Further arguments passed on to the statistic method
#' @examples 
#' \donttest{
#' # Case 1
#' set.seed(1337)
#' exp_data = rexp(8, rate = 1)
#' exp_data
#' bb_sample = bayes_boot(exp_data, mean, n1 = 10000, n2 = 1000)
#' 
#' # Case 2
#' boot_fn = function(cars, weights) {
#'   loess(dist ~ speed, cars, weights = weights)$fitted
#' }
#' 
#' bb_loess = bayes_boot(cars, boot_fn, n1 = 1000, use_weights = TRUE, weight_arg = "weights")
#' 
#' # Plotting the data
#' plot(cars$speed, cars$dist, pch = 20, col = "tomato4", xlab = "Car speed in mph",
#'      ylab = "Stopping distance in ft", main = "Speed and Stopping distances of Cars")
#' 
#' # Plotting a scatter of Bootstrapped LOESS lines to represent the uncertainty.
#' for(i in sample(nrow(bb_loess), 20)) {
#'   lines(cars$speed, bb_loess[i,], col = "gray")
#' }
#' # Finally plotting the posterior mean LOESS line
#' lines(cars$speed, colMeans(bb_loess, na.rm = TRUE), type ="l",
#'       col = "tomato", lwd = 4)
#' }
#' @noRd
bayes_boot = function(data, statistic,
                      n1 = 1000, 
                      n2 = 1000, 
                      use_weights = FALSE, 
                      weight_arg = NULL, ...) {
  # Draw from a uniform Dirichlet dist. with alpha set to rep(1, n_dim).
  # Using the facts that you can transform gamma distributed draws into 
  # Dirichlet draws and that rgamma(n, 1) <=> rexp(n, 1)
  dirichlet_weights = matrix(stats::rexp(NROW(data) * n1, 1) , ncol = NROW(data), byrow = TRUE)
  dirichlet_weights = dirichlet_weights / rowSums(dirichlet_weights)
  
  if(use_weights) {
    stat_call = quote(statistic(data, w, ...))
    names(stat_call)[3] = weight_arg
    boot_sample = apply(dirichlet_weights, 1, function(w) {
      eval(stat_call)
    })
  } else {
    if(is.null(dim(data)) || length(dim(data)) < 2) { # data is a list type of object
      boot_sample = apply(dirichlet_weights, 1, function(w) {
        data_sample = sample(data, size = n2, replace = TRUE, prob = w)
        statistic(data_sample, ...)
      })
    } else { # data is a table type of object
      boot_sample = apply(dirichlet_weights, 1, function(w) {
        index_sample = sample(nrow(data), size = n2, replace = TRUE, prob = w)
        statistic(data[index_sample, ,drop = FALSE], ...)
      })
    }
  }
  if(is.null(dim(boot_sample)) || length(dim(boot_sample)) < 2) {
    # If the bootstrap sample is just a simple vector return it.
    boot_sample
  } else {
    # Otherwise it is a matrix. Since apply returns one row per statistic
    # let's transpose it and return it as a data frame.
    as.data.frame(t(boot_sample))
  }
}

