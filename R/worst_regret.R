#' Worst regret in regret-based models from recursive partitioning trees
#'
#' Compute a worst regret index from recursive partitioning trees. 
#' Regret is an important heuristic in the behavioural sciences. 
#' Minimizing worst regret (the loss under the worst possible 
#' outcome) is a criterion that takes a conservative approach 
#' to risk analysis in diversification strategies. 
#' 
#' @author Jacob van Etten and Kauê de Sousa
#' @param object an object of class \code{party}
#' @param ... further arguments passed to methods
#' @return A data frame with regret estimates
#' \item{items}{the item names}
#' \item{worth}{the worth parameters}
#' \item{worst_regret}{the worst regret}
#' \item{regret}{the squared regret}
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
#' worst_regret(mod)
#' 
#' 
#' @importFrom partykit nodeids
#' @importFrom psychotools itempar
#' @importFrom qvcalc qvcalc.itempar
#' @export
worst_regret <- function(object, ...){
  
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
  
  # get worth from models using qvcalc
  coeffs <- lapply(coeffs, function(X) {
    parameters <- psychotools::itempar(X, vcov = TRUE, alias = TRUE)
    parameters <- qvcalc::qvcalc.itempar(parameters)
    parameters <- parameters[[2]]
    parameters <- as.data.frame(as.matrix(parameters))
  } )

  # get names of items
  items <- unique(row.names(coeffs[[1]]))

  # combine the worth by rows into a single data.frame
  coeffs <- do.call("rbind", coeffs)

  # add node id to the data.frame
  coeffs$node <- rep(nodes, each = length(items))
 
  # add item names
  coeffs$items <- rep(items, times = length(nodes))

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

  rownames(w) <- 1:nrow(w)
  
  class(w) <- union("gosset_df", class(w))
  
  return(w)
  
}
