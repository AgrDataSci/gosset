#' Regret-based values for risk assessment
#'
#' Regret is an important heuristic in the behavioural sciences. 
#' Minimizing worst regret (the loss under the worst possible 
#' outcome) is a criterion that takes a conservative approach 
#' to risk analysis in diversification strategies. 
#' 
#' @author Jacob van Etten and Kauê de Sousa
#' @param object a data.frame, an object of class \code{pltree}, or a 
#'  list with \code{PlackettLuce} models
#' @param bootstrap logical, to run a Bayesian bootstrap on \var{object}
#' @param normalize logical, to normalize values to sum to 1
#' @param group an index in \var{object} for the different scenarios 
#' @param items an index in  \var{object} for the different items
#' @param values an index in \var{object} with the values to compute regret
#' @param node.weights optional named numeric vector with one weight per
#'  scenario in \var{group}, used for worth, regret and their standard
#'  errors. For \code{pltree} objects the weights are the share of
#'  observations in each terminal node, and for lists of models the share
#'  of rankings in each model. Worst regret is not weighted
#' @param ... further arguments passed to methods
#' 
#' @details
#' Additional details for Bayesian bootstrap: 
#' \code{statistic} A function that accepts data as its first argument and possibly, 
#' the weights as its second, if use_weights is TRUE; \code{n1} The size of 
#' the bootstrap sample; \code{n2} The sample size used to calculate 
#' the statistic each bootstrap draw
#' 
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
#' # Case 1 ####
#' library("PlackettLuce")
#' data("breadwheat", package = "gosset")
#' 
#' # convert the tricot rankings from breadwheat data
#' # into a object of class 'grouped_rankings'
#' 
#' G = rank_tricot(breadwheat,
#'                  items = c("variety_a","variety_b","variety_c"),
#'                  input = c("overall_best","overall_worst"),
#'                  group = TRUE)
#' 
#' 
#' # combine grouped rankings with temperature indices
#' mydata = cbind(G, breadwheat[c("lon","lat")])
#' 
#' # fit a pltree model using geographic data
#' mod = pltree(G ~ ., data = mydata)
#' 
#' regret(mod)
#' 
#' # Case 2 ####
#' # list of PlackettLuce models
#' R = matrix(c(1, 2, 3, 0,
#'               4, 1, 2, 3,
#'               2, 1, 3, 4,
#'               1, 2, 3, 0,
#'               2, 1, 3, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) = c("apple", "banana", "orange", "pear")
#' 
#' mod1 = PlackettLuce(R)
#' 
#' R2 = matrix(c(1, 2, 0, 3,
#'                2, 1, 0, 3,
#'                2, 1, 0, 3,
#'                1, 2, 0, 3,
#'                2, 1, 0, 3,
#'                1, 3, 4, 2), nrow = 6, byrow = TRUE)
#' colnames(R2) = c("apple", "banana", "orange", "pear")
#' 
#' mod2 = PlackettLuce(R2)
#' 
#' mod = list(mod1, mod2)
#' 
#' regret(mod, n1 = 500)
#' 
#' @importFrom partykit nodeids
#' @importFrom psychotools itempar
#' @importFrom qvcalc qvcalc.itempar
#' @importFrom stats rexp
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @export
regret = function(object, ..., bootstrap = TRUE, normalize = TRUE) {
  
  UseMethod("regret")
  
}


#' @rdname regret
#' @export
regret.default = function(object, ..., values, items, group,
                          node.weights = NULL,
                          bootstrap = TRUE, normalize = TRUE){
  
  coeffs = object[, c(items, values, group)]
  
  names(coeffs) = c("items", "estimate", "node")
  
  items = unique(coeffs$items)
  
  if (isTRUE(bootstrap)) {
    
    # now the bayes bootstrapping
    coeffs = split(coeffs, list(coeffs$node, coeffs$items), drop = TRUE)
    
    coeffs = lapply(coeffs, function(x){
      
      boots = bayes_boot(x$estimate, mean, ...)
      
      data.frame(estimate = mean(boots), node = x$node[1], items = x$items[1])
      
    })
    
    coeffs = do.call("rbind", coeffs)
    
  }
  
  # regret is difference with the best variety in each node
  # ave() keeps the row order of coeffs; unlist(tapply()) returned the
  # values in node order, which did not match the rows when nodes were
  # not sorted (for example node 10 before node 3 after bootstrapping)
  coeffs$regret = stats::ave(coeffs$estimate, coeffs$node, FUN = function(x) {
    max(x) - x
  })
  
  # weight of each node (scenario). Equal weights reproduce the
  # unweighted results exactly
  if (is.null(node.weights)) {
    coeffs$w = 1
  } else {
    node.weights = unlist(node.weights)
    if (is.null(names(node.weights)) ||
        !all(as.character(unique(coeffs$node)) %in% names(node.weights))) {
      stop("node.weights must be named by node and cover every node in object")
    }
    if (any(!is.finite(node.weights)) || any(node.weights < 0) ||
        sum(node.weights) == 0) {
      stop("node.weights must be finite, non-negative and not all zero")
    }
    coeffs$w = as.numeric(node.weights[as.character(coeffs$node)])
  }
  
  # weighted mean, and standard error of the weighted mean; with equal
  # weights these equal mean(x) and sd(x) / sqrt(n)
  wmean = function(x, w) sum(w * x) / sum(w)
  wse = function(x, w) {
    n = length(x)
    if (n < 2) return(NA_real_)
    w = w / sum(w)
    m = sum(w * x)
    sqrt(n / (n - 1) * sum(w^2 * (x - m)^2))
  }
  
  by_item = split(coeffs, coeffs$items)
  
  # worst regret is the highest regret across the nodes (a worst case,
  # so it is not weighted)
  wr = vapply(by_item, function(d) max(d$regret), numeric(1))
  
  # squared regret summed across nodes, weighted by node; with equal
  # weights this is sum(regret^2)
  regret = vapply(by_item, function(d) {
    nrow(d) * wmean(d$regret^2, d$w)
  }, numeric(1))
  
  se_regret = vapply(by_item, function(d) wse(d$regret^2, d$w), numeric(1))
  
  worth = vapply(by_item, function(d) wmean(d$estimate, d$w), numeric(1))
  
  se_worth = vapply(by_item, function(d) wse(d$estimate, d$w), numeric(1))
  
  w = data.frame(items = items, 
                 worth = worth[items], 
                 worst_regret = wr[items], 
                 regret = regret[items],
                 worthSE = se_worth[items],
                 regretSE = se_regret[items])
  
  w = w[order(w$regret), ]
  
  if (isTRUE(normalize)) {
    # normalize to sum to 1
    w[2:4] = lapply(w[2:4], function(x) {
      nrm = 1 / sum(x)
      x * nrm
    })
  }
  
  rownames(w) = 1:nrow(w)
  
  class(w) = union("gosset_df", class(w))
  
  return(w)
  
}

#' @rdname regret
#' @method regret pltree
#' @export
regret.pltree = function(object, bootstrap = TRUE, normalize = TRUE, ...) {
  
  # get ids of terminal nodes
  nodes = partykit::nodeids(object, terminal = TRUE)
  
  # get the models from each terminal node
  coeffs = list()
  for(i in seq_along(nodes)) {
    coeffs[[i]] = object[[ nodes[i] ]]$node$info$object
  }
  
  # probability of the scenario is the weigthed values of 
  # number of observations in the nodes
  # get number of observations in each inner node
  nobs = integer(0L)
  for (i in seq_along(nodes)) {
    nobs = c(nobs, as.integer(object[[nodes[i]]]$node$info$nobs))
  }
  
  probs = nobs / sum(nobs)
  
  if (isTRUE(bootstrap)) {
    ci_level = 1 - object$info$control$alpha
    # get worth from models using qvcalc
    coeffs = lapply(coeffs, function(X) {
      pars = psychotools::itempar(X, log = FALSE)
      pars = qvcalc::qvcalc(pars)$qvframe
      pars = as.data.frame(as.matrix(pars))
      pars$items = rownames(pars)
      # add confidence intervals as "new" data sets for bootstrapping 
      pars1 = pars
      pars1$estimate = pars1$estimate + stats::qnorm(1 - (1 - ci_level) / 2) * pars1$quasiSE
      pars2 = pars
      pars2$estimate = pars2$estimate - stats::qnorm(1 - (1 - ci_level) / 2) * pars2$quasiSE
      pars = rbind(pars, pars1, pars2)
      pars
    })
    
    # get names of items
    items = unique(coeffs[[1]]$items)
    
    # combine the worth by rows into a single data.frame
    coeffs = do.call("rbind", coeffs)
    
    # add node id to the data.frame
    coeffs$node = rep(nodes, each = length(items) * 3)
  }
  
  if (isFALSE(bootstrap)) {
    # get worth from models using qvcalc
    coeffs = lapply(coeffs, function(X) {
      pars = psychotools::itempar(X, vcov = FALSE, alias = TRUE)
      pars = qvcalc::qvcalc.itempar(pars)
      pars = pars[[2]]
      pars = as.data.frame(as.matrix(pars))
      pars$items = rownames(pars)
      pars
    })
    
    # get names of items
    items = unique(coeffs[[1]]$items)
    
    # combine the worth by rows into a single data.frame
    coeffs = do.call("rbind", coeffs)
    
    # add node id to the data.frame
    coeffs$node = rep(nodes, each = length(items))
  }
  
  regret(object = coeffs,
         values = "estimate",
         items = "items",
         group = "node",
         node.weights = stats::setNames(probs, nodes),
         bootstrap = bootstrap,
         normalize = normalize, 
         ...)
  
}

#' @rdname regret
#' @method regret list
#' @export
regret.list = function(object, bootstrap = TRUE, normalize = TRUE, ...) {
  
  isPlackettLuce = unlist(lapply(object, class))
  
  if (!"PlackettLuce" %in% isPlackettLuce) {
    stop("Only objects of class 'PlackettLuce' are accepted for this method \n")
  }
  
  if (isTRUE(bootstrap)) {
    ci_level = 1 - 0.05
    # is likely that the error id due to missing items in one the nodes
    # so we apply the function pseudo_ranking() to add these missing items
    # extract the original rankings, add pseudo_ranking and refit the model
    coeffs = lapply(object, function(y){
      r = y$rankings
      r = pseudo_rank(r)
      stats::update(y, rankings = r)
    })
    
    # get worth from models using qvcalc
    coeffs = lapply(coeffs, function(X) {
      pars = psychotools::itempar(X, log = FALSE)
      # get estimates from item parameters using qvcalc
      pars = qvcalc::qvcalc(pars)$qvframe
      pars$quasiSE[is.na(pars$quasiSE)] = min(pars$quasiSE, na.rm = TRUE)
      pars$items = rownames(pars)
      # add confidence intervals as "new" data sets for bootstrapping
      pars1 = pars
      pars1$estimate = pars1$estimate + stats::qnorm(1 - (1 - ci_level) / 2) * pars1$quasiSE
      pars2 = pars
      pars2$estimate = pars2$estimate - stats::qnorm(1 - (1 - ci_level) / 2) * pars2$quasiSE
      pars = rbind(pars, pars1, pars2)
      pars
    })
    
    # get names of items
    items = unique(coeffs[[1]]$items)
    
    # combine the worth by rows into a single data.frame
    coeffs = do.call("rbind", coeffs)
    
    # add node id to the data.frame
    coeffs$node = rep(1:length(object), each = length(items) * 3)
    
  }
  
  if (isFALSE(bootstrap)) {
    # get worth from models using qvcalc
    coeffs = lapply(object, function(y){
      r = y$rankings
      r = pseudo_rank(r)
      m = stats::update(y, rankings = r)
      pars = psychotools::itempar(m, log = FALSE)
      pars = qvcalc::qvcalc(pars)$qvframe
      pars$quasiSE[is.na(pars$quasiSE)] = min(pars$quasiSE, na.rm = TRUE)
      pars$items = rownames(pars)
      pars
    })
    
    # get names of items
    items = unique(coeffs[[1]]$items)
    
    # combine the worth by rows into a single data.frame
    coeffs = do.call("rbind", coeffs)
    
    # add node id to the data.frame
    coeffs$node = rep(1:length(object), each = length(items))
    
  }
  
  # weight each model by its number of rankings
  nobs = vapply(object, function(y) as.numeric(nrow(y$rankings)), numeric(1))
  
  regret(object = coeffs,
         values = "estimate",
         items = "items",
         group = "node",
         node.weights = stats::setNames(nobs / sum(nobs), seq_along(object)),
         bootstrap = bootstrap,
         normalize = normalize)
}

