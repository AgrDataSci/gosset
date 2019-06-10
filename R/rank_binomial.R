#' Binary rankings
#'
#' Pairwise comparisons from a ranking object. Ties are not taken into account, then they are added as NA's.
#'
#' @param object an object of class "rankings", "grouped_rankings" or "paircomp".
#' @param drop.null logical, an optional argument to remove players without comparisons 
#' @return a binary rank
#' @seealso \code{\link[PlackettLuce]{rankings}}, \code{\link[psychotools]{paircomp}}
#' @examples 
#' \notrun{}
#' @export
rank_binomial <- function(object, drop.null = FALSE) 
{
  
  if (.is_grouped_rankings(object)) {
    object <- to_paircomp(object)
  }
  
  if (.is_rankings(object)) {
    object <- to_paircomp(object)
  }
  
  # take all possible combinations between items
  p_labels <- t(.combn2(labels(object), 2))
  
  # paired comparisons into dataframe
  object <- cbind(p_labels, data.frame(summary(object))[1:2])
  
  # > win1 and < win2 
  #select vectors to keep on the dataframe
  names(object) <- c("player1","player2","win1","win2")
  
  # rename row names in object
  row.names(object) <- 1:nrow(object)
  
  # remove combinations without comparisons
  if (drop.null) {
    drop <- object$win1 == 0 & object$win2 == 0
    object <- object[!drop, ]
  }
  
  # take the name of all items
  players <- factor(sort(unique(as.vector(unlist(object[,c("player1","player2")])))))
  
  # take all possible combinations between items
  #comb <- t(.combn2(players, 2))
  
  # generate two vector with equal factors corresponding to players
  #player1 <- factor(players[comb[, 1]], levels = players)
  #player2 <- factor(players[comb[, 2]], levels = players)
  
  # run over object looking for corresponding combination and 
  # add the results into the new dataframe object_bin
  object$player1 <- factor(object$player1, levels = players)
  object$player2 <- factor(object$player2 , levels = players)
  
  object <- tibble::as_tibble(object)
  
  return(object)
  
}
