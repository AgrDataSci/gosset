#' Pairwise victories
#' 
#' Summarise victories from pairwise comparisons
#' 
#' @author Nicolas Greliche, Sam Dumble and Kauê de Sousa
#' @family summarise functions
#' @param object a rankings object of class 'rankings', 'grouped_rankings' 
#' or 'paircomp'
#' @param x an object of class 'gosset_vctr' for the plotting method. 
#' Generates a 'ggplot' object that can be passed to any ggplot2 method
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
#'                  items = c("variety_a", "variety_b", "variety_c"),
#'                  input = c("overall_best", "overall_worst"))
#' 
#' 
#' v <- summarise_victories(R)
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
#'                  items = c("variety_a", "variety_b", "variety_c"),
#'                  input = c("best", "worst"),
#'                  additional.rank = beans[, c("var_a", "var_b", "var_c")])
#' 
#' v <- summarise_victories(R)
#'   
#' @importFrom ggplot2 ggplot geom_bar aes facet_wrap coord_flip 
#'  scale_fill_gradient2 scale_y_continuous labs
#' @export
summarise_victories <- function(object){

  # get binomial rankings
  bin <- rank_binomial(object, drop.null = TRUE)
  
  # factors into character
  bin[1:2] <- lapply(bin[1:2], as.character)

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
  class(bin) <- c("gosset_vctr", class(bin))
  
  return(bin)
  
}


#' @rdname summarise_victories
#' @method plot gosset_vctr
#' @export
plot.gosset_vctr <- function(x, ...) {
  

  # get order of players based on their performance
  player_levels <- .player_order(x, "player1", "victories")
  
  # set levels to player1 and player2
  x$player1 <- factor(x$player1, levels = player_levels)
  x$player2 <- factor(x$player2, levels = player_levels)
  
  x$victories <- x$victories * 100
  
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
      limits = c(0, 100),
      labels = paste0(seq(0, 100, by = 25), "%"),
      breaks = seq(0, 100, by = 25),
      low =  "#FFFFFF",
      high =  "#0571B0",
      name = ""
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 100),
                                labels = paste0(seq(0, 100, by = 25), "%"),
                                breaks = seq(0, 100, by = 25)) + 
    ggplot2::labs(x = "Player", y = "Relative victories")

  return(p)
}