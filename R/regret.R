#' Regret-based values for risk assessment
#'
#' Regret is an important heuristic in the behavioural sciences. 
#' Minimizing worst regret (the loss under the worst possible 
#' outcome) is a criterion that takes a conservative approach 
#' to risk analysis in diversification strategies. 
#' 
#' @author Jacob van Etten and Kauê de Sousa
#' @param object a data.frame or an object of class \code{pltree}
#' @param bootstrap logical, to run a bayes bootstrap on \var{object}
#' @param normalize logical, to normalize values to sum to 1
#' @param group an index in \var{object} for the different scenarios 
#' @param items an index in  \var{object} for the different items
#' @param values an index in \var(object) with the values to compute regret
#' @param ... further arguments passed to methods
#' @return A data frame with regret estimates
#' \item{items}{the item names}
#' \item{worth}{the worth parameters}
#' \item{regret}{the squared regret}
#' \item{worst_regret}{the worst regret}
#' @references 
#' Loomes G. & Sugden R. (1982). 
#' The Economic Journal, 92(368), 805. 
#' \doi{https://doi.org/10.2307/2232669}
#' 
#' Bleichrodt H. & Wakker P. P. (2015). 
#' The Economic Journal, 125(583), 493–532. 
#' \doi{https://doi.org/10.1111/ecoj.12200}
#' 
#' @examples
#'
#' library("PlackettLuce")
#' data("breadwheat", package = "gosset")
#' 
#' # convert the tricot rankings from breadwheat data
#' # into a object of class 'grouped_rankings'
#' 
#' G <- rank_tricot(breadwheat,
#'                  items = c("variety_a","variety_b","variety_c"),
#'                  input = c("overall_best","overall_worst"),
#'                  group = TRUE)
#' 
#' 
#' # combine grouped rankings with temperature indices
#' mydata <- cbind(G, breadwheat[c("lon","lat")])
#' 
#' # fit a pltree model using geographic data
#' mod <- pltree(G ~ ., data = mydata)
#' 
#' regret(mod)
#' 
#' 
#' @importFrom partykit nodeids
#' @importFrom psychotools itempar
#' @importFrom qvcalc qvcalc.itempar
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @export
regret <- function(object, ..., bootstrap = TRUE, normalize = TRUE) {
  
  UseMethod("regret")
  
}


#' @rdname regret
#' @export
regret.default <- function(object, ..., values, items, group,
                           bootstrap = TRUE, normalize = TRUE){
  
  coeffs <- object[, c(items, values, group)]
  
  names(coeffs) <- c("items", "estimate", "node")
  
  items <- unique(coeffs$items)
  
  if (isTRUE(bootstrap)) {
    
    # now the bayes bootstrapping
    coeffs <- split(coeffs, paste0(coeffs$node, coeffs$items))
    
    coeffs <- lapply(coeffs, function(x){
      
      boots <- bayes_boot(x$estimate, mean, ...)
      
      data.frame(estimate = mean(boots), node = x$node[1], items = x$items[1])
      
    })
    
    coeffs <- do.call("rbind", coeffs)
    
  }
  
  # regret is difference with the best variety in each node
  coeffs$regret <- unlist(tapply(coeffs$estimate, coeffs$node, function(x) {
    max(x) - x
  }))

  # worst regret is the highest regret across the nodes
  wr <- tapply(coeffs$regret, coeffs$items, max)

  # regret is the sum of the squared values of all items regret
  regret <- unlist(tapply(coeffs$regret, coeffs$items, function(x) {
    sum(x)^2
  }))
  
  worth <- tapply(coeffs$estimate, coeffs$items, mean)
  
  w <- data.frame(items = items, 
                  worth = worth[items], 
                  worst_regret = wr[items], 
                  regret = regret[items])

  w <- w[order(w$regret), ]
  
  if (isTRUE(normalize)) {
    # normalize to sum to 1
    w[2:4] <- lapply(w[2:4], function(x) {
      nrm <- 1 / sum(x)
      x * nrm
    })
  }
  
  rownames(w) <- 1:nrow(w)
  
  class(w) <- union("gosset_df", class(w))
  
  return(w)
  
}

#' @rdname regret
#' @method regret pltree
#' @export
regret.pltree <- function(object, bootstrap = TRUE, normalize = TRUE, ...) {
  
  # get ids of terminal nodes
  nodes <- partykit::nodeids(object, terminal = TRUE)
  
  # get the models from each terminal node
  coeffs <- list()
  for(i in seq_along(nodes)) {
    coeffs[[i]] <- object[[ nodes[i] ]]$node$info$object
  }
  
  # probability of the scenario is the weigthed values of 
  # number of observations in the nodes
  # get number of observations in each inner node
  nobs <- integer(0L)
  for (i in seq_along(nodes)) {
    nobs <- c(nobs, as.integer(object[[nodes[i]]]$node$info$nobs))
  }
  
  probs <- nobs / sum(nobs)
  
  if (isTRUE(bootstrap)) {
    ci_level <- 1 - object$info$control$alpha
    # get worth from models using qvcalc
    coeffs <- lapply(coeffs, function(X) {
      pars <- psychotools::itempar(X, vcov = FALSE, alias = TRUE)
      pars <- qvcalc::qvcalc.itempar(pars)
      pars <- pars[[2]]
      pars <- as.data.frame(as.matrix(pars))
      pars$items <- rownames(pars)
      # add confidence intervals as "new" data sets for bootstrapping 
      pars1 <- pars
      pars1$estimate <- pars1$estimate + stats::qnorm(1 - (1 - ci_level) / 2) * pars1$quasiSE
      pars2 <- pars
      pars2$estimate <- pars2$estimate - stats::qnorm(1 - (1 - ci_level) / 2) * pars2$quasiSE
      pars <- rbind(pars, pars1, pars2)
      pars
    })
    
    # get names of items
    items <- unique(coeffs[[1]]$items)
    
    # combine the worth by rows into a single data.frame
    coeffs <- do.call("rbind", coeffs)
    
    # add node id to the data.frame
    coeffs$node <- rep(nodes, each = length(items) * 3)
  }
  
  if (isFALSE(bootstrap)) {
    # get worth from models using qvcalc
    coeffs <- lapply(coeffs, function(X) {
      pars <- psychotools::itempar(X, vcov = FALSE, alias = TRUE)
      pars <- qvcalc::qvcalc.itempar(pars)
      pars <- pars[[2]]
      pars <- as.data.frame(as.matrix(pars))
      pars$items <- rownames(pars)
      pars
    })
    
    # get names of items
    items <- unique(coeffs[[1]]$items)
    
    # combine the worth by rows into a single data.frame
    coeffs <- do.call("rbind", coeffs)
    
    # add node id to the data.frame
    coeffs$node <- rep(nodes, each = length(items))
  }
  
  regret(object = coeffs,
         values = "estimate",
         items = "items",
         group = "node",
         bootstrap = bootstrap,
         normalize = normalize, 
         ...)
  
}


# regret.list <- function(object, bootstrap = TRUE, normalize = TRUE, ...) {
#   
#   isPlackettLuce <- unlist(lapply(object, class))
#   
#   if (!"PlackettLuce" %in% isPlackettLuce) {
#     
#     
#     
#   }
  
  # # get ids of terminal nodes
  # nodes <- partykit::nodeids(object, terminal = TRUE)
  # 
  # # get the models from each terminal node
  # coeffs <- list()
  # for(i in seq_along(nodes)) {
  #   coeffs[[i]] <- object[[ nodes[i] ]]$node$info$object
  # }
  # 
  # # probability of the scenario is the weigthed values of 
  # # number of observations in the nodes
  # # get number of observations in each inner node
  # nobs <- integer(0L)
  # for (i in seq_along(nodes)) {
  #   nobs <- c(nobs, as.integer(object[[nodes[i]]]$node$info$nobs))
  # }
  # 
  # probs <- nobs / sum(nobs)
  # 
  # if (isTRUE(bootstrap)) {
  #   ci_level <- 1 - object$info$control$alpha
  #   # get worth from models using qvcalc
  #   coeffs <- lapply(coeffs, function(X) {
  #     pars <- psychotools::itempar(X, vcov = FALSE, alias = TRUE)
  #     pars <- qvcalc::qvcalc.itempar(pars)
  #     pars <- pars[[2]]
  #     pars <- as.data.frame(as.matrix(pars))
  #     pars$items <- rownames(pars)
  #     # add confidence intervals as "new" data sets for bootstrapping 
  #     pars1 <- pars
  #     pars1$estimate <- pars1$estimate + stats::qnorm(1 - (1 - ci_level) / 2) * pars1$quasiSE
  #     pars2 <- pars
  #     pars2$estimate <- pars2$estimate - stats::qnorm(1 - (1 - ci_level) / 2) * pars2$quasiSE
  #     pars <- rbind(pars, pars1, pars2)
  #     pars
  #   })
  #   
  #   # get names of items
  #   items <- unique(coeffs[[1]]$items)
  #   
  #   # combine the worth by rows into a single data.frame
  #   coeffs <- do.call("rbind", coeffs)
  #   
  #   # add node id to the data.frame
  #   coeffs$node <- rep(nodes, each = length(items) * 3)
  # }
  # 
  # if (isFALSE(bootstrap)) {
  #   # get worth from models using qvcalc
  #   coeffs <- lapply(coeffs, function(X) {
  #     pars <- psychotools::itempar(X, vcov = FALSE, alias = TRUE)
  #     pars <- qvcalc::qvcalc.itempar(pars)
  #     pars <- pars[[2]]
  #     pars <- as.data.frame(as.matrix(pars))
  #     pars$items <- rownames(pars)
  #     pars
  #   })
  #   
  #   # get names of items
  #   items <- unique(coeffs[[1]]$items)
  #   
  #   # combine the worth by rows into a single data.frame
  #   coeffs <- do.call("rbind", coeffs)
  #   
  #   # add node id to the data.frame
  #   coeffs$node <- rep(nodes, each = length(items))
  # }
  # 
  # regret(object = coeffs,
  #        values = "estimate",
  #        items = "items",
  #        group = "node",
  #        bootstrap = bootstrap,
  #        normalize = normalize, 
  #        ...)
  
# }

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
#' # Case 1
#' set.seed(1337)
#' exp_data <- rexp(8, rate = 1)
#' exp_data
#' bb_sample <- bayes_boot(exp_data, mean, n1 = 10000, n2 = 1000)
#' 
#' # Case 2
#' boot_fn <- function(cars, weights) {
#'   loess(dist ~ speed, cars, weights = weights)$fitted
#' }
#' 
#' bb_loess <- bayes_boot(cars, boot_fn, n1 = 1000, use_weights = TRUE, weight_arg = "weights")
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
#'  
#'  @noRd
bayes_boot <- function(data, statistic,
                       n1 = 1000, 
                       n2 = 1000, 
                       use_weights = FALSE, 
                       weight_arg = NULL, ...) {
  # Draw from a uniform Dirichlet dist. with alpha set to rep(1, n_dim).
  # Using the facts that you can transform gamma distributed draws into 
  # Dirichlet draws and that rgamma(n, 1) <=> rexp(n, 1)
  dirichlet_weights <- matrix( rexp(NROW(data) * n1, 1) , ncol = NROW(data), byrow = TRUE)
  dirichlet_weights <- dirichlet_weights / rowSums(dirichlet_weights)
  
  if(use_weights) {
    stat_call <- quote(statistic(data, w, ...))
    names(stat_call)[3] <- weight_arg
    boot_sample <- apply(dirichlet_weights, 1, function(w) {
      eval(stat_call)
    })
  } else {
    if(is.null(dim(data)) || length(dim(data)) < 2) { # data is a list type of object
      boot_sample <- apply(dirichlet_weights, 1, function(w) {
        data_sample <- sample(data, size = n2, replace = TRUE, prob = w)
        statistic(data_sample, ...)
      })
    } else { # data is a table type of object
      boot_sample <- apply(dirichlet_weights, 1, function(w) {
        index_sample <- sample(nrow(data), size = n2, replace = TRUE, prob = w)
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
