#' Pairwise player dominance
#' 
#' Methods to summarise player dominance from pairwise comparisons
#' 
#' @param object a rankings object of class 'rankings', 'grouped_rankings'
#'  or 'paircomp'
#' @param x an object of class 'gosset_dmnc' for the plotting method. 
#' Generates a 'ggplot' object that can be passed to any ggplot2 method
#' @param ... further arguments passed to methods. Not enabled yet
#' @return A data.frame with summary of dominance from pairwise comparisons: 
#' \item{player1}{the first player in the comparison}
#' \item{player2}{the second player in the comparison}
#' \item{ncontest}{number of contests between player1 and player2}
#' \item{dominance}{relative dominance of player1 on player2}
#' @family recap functions
#' @examples 
#' # breadwheat data
#' 
#' data("breadwheat", package = "gosset")
#'  
#' R <- rank_tricot(data = breadwheat,
#'                  items = c("variety_a", "variety_b", "variety_c"),
#'                  input = c("overall_best", "overall_worst"))
#' 
#' d <- dominance(R)
#' 
#' p <- plot(d)
#' 
#' # beans data where each observer compares 3 varieties
#' # randomly distributed
#' 
#' library("PlackettLuce")
#' data("beans", package = "PlackettLuce")
#' 
#' R <- rank_tricot(data = beans,
#'                  items = c("variety_a", "variety_b", "variety_c"),
#'                  input = c("best", "worst"),
#'                  additional.rank = beans[, c("var_a", "var_b", "var_c")])
#' 
#' d <- recap_dominance(R)
#' 
#'  
#' @export
recap_dominance <- function(object){

  # get binomial rankings
  bin <- rank_binomial(object)
  
  # factors into character
  bin[1:2] <- lapply(bin[1:2], as.character)
  
  bin$ncontest <- bin$win1 + bin$win2
  
  names(bin) <- gsub("1|2","", names(bin))
  
  bin <- rbind(bin[,c(1,2,3,5)],
               bin[,c(2,1,4,5)])
  
  # rename variables
  names(bin) <- c("player1","player2","win","ncontest")
  
  # calculate preference
  bin$dominance <- 100 * (bin$win / bin$ncontest)
  
  # reorder by player1 for a visually better output
  bin <- bin[order(bin$player1), ]
  
  bin <- bin[, -c(3)]
  
  # add class for the plotting method
  class(bin) <- c("gosset_dmnc", class(bin))
  
  return(bin)
  
}


#' @rdname recap_dominance
#' @method plot gosset_dmnc
#' @export
plot.gosset_dmnc <- function(x, ...) {
  
  # get order of players based on their performance
  player_levels <- .player_order(x, "player1", "dominance")
  
  # set levels to player1 and player2
  x$player1 <- factor(x$player1, levels = player_levels)
  x$player2 <- factor(x$player2, levels = player_levels)
  
  p <-   ggplot2::ggplot(x, 
                         ggplot2::aes(x = x$player2, 
                                      y = x$player1,
                                      fill = x$dominance,
                                      label = round(x$dominance, 0))) +
    ggplot2::geom_tile() + 
    ggplot2::geom_text(size = 3, fontface = 2) +
    ggplot2::scale_x_discrete(position = "top")+
    ggplot2::scale_fill_gradient2(limits = c(0, 100),
                                  low =  "#FFFFFF",
                                  high =  "#0571B0") +
    ggplot2::labs(x = "Player 2", 
                  y = "Player 1",
                  fill="Relative dominance of player1")

  return(p)
}