#' Pairwise victories
#' 
#' Summarise victories from pairwise comparisons
#' 
#' @param object a rankings object of class 'rankings', 'grouped_rankings' or 'paircomp'
#' @param x an object of class 'gosset_vtr' for the plotting method. Generates a 'ggplot' object that can be passed to any ggplot2 method
#' @param ... further arguments passed to methods. Not enabled yet
#' @return A data.frame with summary of victories from pairwise comparisons: 
#' \item{player1}{the first player in the comparison}
#' \item{player2}{the second player in the comparison}
#' \item{win}{times player1 wins against player2}
#' \item{ncontest}{number of contests between player1 and player2}
#' \item{victories}{relative number of times player1 wins against player2}
#' @examples 
#' # breadwheat data
#' 
#' data("breadwheat", package = "gosset")
#' 
#' R <- rank_tricot(data = breadwheat,
#'                  items = c(1:3),
#'                  input = c(18:19))
#' 
#' v <- victories(R)
#' 
#' p <- plot(v)
#' 
#' # beans data where each observer compares 3 varieties
#' # randomly distributed
#' 
#' library("PlackettLuce")
#' data("beans", package = "PlackettLuce")
#' 
#' R <- rank_tricot(data = beans,
#'                  items = c(1:3),
#'                  input = c(4:5),
#'                  additional.rank = beans[c(6:8)])
#' 
#' v <- victories(R)
#' 
#'  
#' @importFrom scales percent
#' @importFrom stats aggregate
#' @export
victories <- function(object){

  # get binomial rankings
  bin <- rank_binomial(object)

  bin$ncontest <- bin$win1 + bin$win2

  names(bin) <- gsub("1|2","", names(bin))

  bin <- rbind(bin[,c(1,2,3,5)],
               bin[,c(2,1,4,5)])

  # rename variables
  names(bin) <- c("player1","player2","win","ncontest")
  
  # calculate preference
  bin$victories <- (bin$win / bin$ncontest)
  
  # reorder by player1 for a visually better output
  bin <- bin[order(bin$player1), ]
  
  # add class for the plotting method
  class(bin) <- c("gosset_vtr", class(bin))
  
  return(bin)
  
}


#' @rdname victories
#' @method plot gosset_vtr
#' @export
plot.gosset_vtr <- function(x, ...) {
  
  # get summary to order items from higher to lower
  x_mean <- stats::aggregate(x$victories, 
                             by = list(x$player1),
                             mean)
  # get order
  player_levels <- rev(order(x_mean[,2]))
  
  # define levels
  player_levels <- x_mean[player_levels, 1]
  
  # set levels to player1 and player2
  x$player1 <- factor(x$player1, levels = player_levels)
  x$player2 <- factor(x$player2, levels = player_levels)
  
  
  p <- 
  ggplot2::ggplot(x) +
    ggplot2::geom_bar(ggplot2::aes(y = x$victories, 
                                   x = x$player2,
                                   fill = x$victories), 
                      stat = "identity", 
                      col = "black") +
    ggplot2::facet_wrap(. ~ x$player1,
                        scales = "free_y", 
                        ncol = 3) +
    ggplot2::coord_flip() + 
    ggplot2::scale_fill_gradient2(
      limits = c(0, 1),
      low = alpha("#FFFFFF", 0.90),
      high = alpha("#0571B0", 0.90),
      labels = scales::percent,
      name = ""
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent, 
                                limits = c(0, 1)) + 
    labs(x = "Player", y = "Relative victories")

  return(p)
}