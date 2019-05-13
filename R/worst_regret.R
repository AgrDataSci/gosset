#' Worst regret in regret-based models from recursive partitioning trees
#'
#' Compute a worst regret index from recursive partitioning trees. Regret is an important
#' heuristic in the behavioural sciences. Minimizing worst regret (the loss under the worst
#' possible outcome) is a criterion that takes a conservative approach to risk analysis in 
#' diversification strategies. 
#'
#' @param object an object of class modelparty
#' @param ... further arguments passed to methods
#' @return A data frame with probabilities of winning
#' \item{items}{the item names}
#' \item{win_probs}{the probabilities of winning}
#' \item{worst_regret}{the worst regret index}
#' @examples
#'
#' library("PlackettLuce")
#' 
#' data("breadwheat", package = "gosset")
#' 
#' # convert the tricot rankings from breadwheat data
#' # into a object of class 'grouped_rankings'
#' 
#' G <- to_rankings(breadwheat,
#'                  items = c("variety_a","variety_b","variety_c"),
#'                  input = c("overall_best","overall_worst"),
#'                  type = "tricot",
#'                  grouped.rankings = TRUE)
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
#' @export
worst_regret <- function(object, ...){
  
  # get ids of terminal nodes
  nodes <- partykit::nodeids(object, terminal = TRUE)

  # get the coefficients from each terminal node
  probs <- list()
  for(i in seq_along(nodes)) probs[[i]] <- object[[ nodes[i] ]]$node$info$object

  # get estimates from terminal nodes using qvcalc
  probs <- lapply(probs, function(X) {

    parameters <- psychotools::itempar(X, vcov = TRUE, alias = TRUE)
    parameters <- qvcalc::qvcalc.itempar(parameters)
    parameters <- parameters[[2]]
    parameters <- as.data.frame(as.matrix(parameters))
  } )

  # get names of items
  items <- unique(row.names(probs[[1]]))

  # combine these probabilites by rows into a single dataframe
  probs <- do.call("rbind", probs)

  # add node id to the dataframe
  probs$node <- rep(nodes, each = length(items))

  # add item names
  probs$items <- rep(items, times = length(nodes))

  # change row names to numeric
  row.names(probs) <- 1:nrow(probs)

  # regret is difference with the best variety in each node
  probs$regret <- unlist(tapply(probs$estimate, probs$node, function(x) {
    max(x) - x
  }))

  # worst regret is the highest regret across the nodes
  WR <- tapply(probs$regret, probs$items, max)

  # get the probability of winning of all observers
  # first, check if winprobs is provided as input 
  dots <- list(...)
  if("winprobs" %in% names(dots)) { 
    winprobs <- dots[["winprobs"]]  
  } else {
    winprobs <- predict(object)  
  }
  
  winProbs <- apply(winprobs, 2, mean)

  w <- as.data.frame(cbind(win_probs = winProbs , worst_regret = WR ))

  w$items <- row.names(w)

  row.names(w) <- 1:nrow(w)
  
  w <- dplyr::arrange(w, worst_regret)

  w <- tibble::as_tibble(w[ ,c(3,1:2)])

  return(w)

}