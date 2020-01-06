#' Binary rankings from pairwise contests
#'
#' Binary comparisons from a ranking object. Ties are not taken into account, 
#' then they are added as NA's.
#'
#' @param object an object of class "rankings", "grouped_rankings" or "paircomp".
#' @param drop.null logical, an optional argument to remove null contests 
#' @return A data.frame with binary rank of pairwise contests:
#' \item{player1}{a factor with n levels for the first player in the contests}
#' \item{player2}{a factor with n levels (same as player1) for the second player in the contests}
#' \item{win1}{number of times player1 wins against player2}
#' \item{win2}{number of times player2 wins against player1}
#' @family rank functions
#' @seealso \code{\link[BradleyTerry2]{BTm}}
#' @references 
#' Turner H. & Firth D. (2012). Journal of Statistical Software, 48(9), 1–21. http://dx.doi.org/10.18637/jss.v048.i09
#' 
#' @examples 
#' \donttest{
#' library("PlackettLuce")
#' 
#' # a simple matrix with 4 items
#' # ties are computed as NA's
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 4, 3, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' R <- PlackettLuce::as.rankings(R)
#' 
#' R <- rank_binomial(R)
#' }
#' @export
rank_binomial <- function(object, drop.null = FALSE) 
{
  
  if (.is_grouped_rankings(object) | .is_rankings(object)) {
    object <- rank_paircomp(object)
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
  
  # run over object looking for corresponding combination and 
  # add the results into the new dataframe object_bin
  object$player1 <- factor(object$player1, levels = players)
  object$player2 <- factor(object$player2 , levels = players)
  
  object <- tibble::as_tibble(object)
  
  return(object)
  
}
