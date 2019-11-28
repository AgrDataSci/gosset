#' Pairwise preference
#' 
#' Compute pairwise preference matrix between comparisons
#' 
#' @inheritParams rank_tricot
#' @param x an object of class 'cmb_pref' for the plotting method. Generates a 'ggplot' object that can be passed to any ggplot2 method
#' @return a data.frame with pairwise comparisons 
#' \item{player1}{the first player in the comparison}
#' \item{player2}{the second player in the comparison}
#' \item{win}{times player1 wins against player2}
#' \item{ncontest}{number of contests between player1 and player2}
#' \item{preference}{number of times player1 is preferred against player2}
#' @examples 
#' 
#' # breadwheat data
#' 
#' data("breadwheat", package = "gosset")
#' 
#' preference(data = breadwheat,
#'            items = c(1:3),
#'            input = c(18:19))
#' 
#' # beans data where each observer compares 3 varieties
#' # randomly distributed
#' 
#' library("PlackettLuce")
#' data("beans", package = "PlackettLuce")
#' 
#' preference(data = beans,
#'            items = c(1:3),
#'            input = c(4:5))
#' 
#' # also include the local item
#' preference(data = beans,
#'            items = c(1:3),
#'            input = c(4:5),
#'            additional.rank = beans[c(6:8)])
#'    
#' @importFrom scales percent
#' @importFrom stats aggregate
#' @export
preference <- function(data = NULL, items = NULL, input = NULL, ...){

  # check data
  data <- .check_data(data = data,
                      items = items, 
                      input = input)
  # take rankings
  # this allows the function to accept other arguments passed
  # to build_rankings, such as including the comparison with the
  # local item
  dots <- list(...)
  args <- c(data, dots)

  R <- do.call("rank_tricot", args)  

  # THERE IS AN EASIER SOLUTION USING gosset, SEE BELLOW
  # BUT WE HAVE TO WAIT UNTIL THE PACKAGE IS READY TO PUBLISH IN CRAN
  # get adjacency matrix to see how many times each item wins
  bin <- .rank_binomial(R)
  
  # get number of contests
  bin$ncontest <- bin$win1 + bin$win2
  
  bin <- bin[,c("player1","player2","win1","ncontest")]
  
  # rename variables
  names(bin) <- c("player1","player2","win","ncontest")
  
  # calculate preference
  bin$preference <- (bin$win / bin$ncontest)
  
  # reorder by player1 for a visually better output
  bin <- bin[order(bin$player1), ]
  
  # add class for the plotting method
  class(bin) <- c("cmb_pref", class(bin))
  
  return(bin)
  
}


#' @rdname preference
#' @method plot cmb_pref
#' @export
plot.cmb_pref <- function(x, ...) {
  
  # get summary to order items from higher to lower
  x_mean <- stats::aggregate(x$preference, 
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
    ggplot2::geom_bar(ggplot2::aes(y = x$preference, 
                                   x = x$player2,
                                   fill = x$preference), 
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
      name = "% Times preferred"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent, 
                                limits = c(0, 1)) + 
    labs(x = "Player", y = "")

  return(p)
}

# temporary solution for pairwise comparisons
# future code bellow from gosset package
.rank_binomial <- function(data) {
  
  adj <- PlackettLuce::adjacency(data)
  
  adj <- data.frame(adj, check.names = FALSE)
  
  adj$player1 <- rownames(adj)
  
  adj <- tidyr::gather(adj, -c("player1"), key = "player2", value = "win1")
  
  # get the transversal matrix to see how many contests occurred 
  adj_t <- t(PlackettLuce::adjacency(data))
  
  adj_t <- data.frame(adj_t, check.names = FALSE)
  
  adj_t$player1 <- rownames(adj_t)
  
  adj_t <- tidyr::gather(adj_t, -c("player1"), key = "player2", value = "win2")
  
  
  # add winnings of player2 to the main data.frame
  adj$win2 <- adj_t$win2
  
  adj <- adj[adj$player1 != adj$player2, ]
  
  adj <- tibble::as_tibble(adj)
  
  return(adj)
}


# # # get pairwise 
# # # this function comes from the package gosset
# # # I will hide it here until gosset is published
# bin <- gosset::rank_binomial(R)
# 
# bin$ncontest <- bin$win1 + bin$win2
# 
# names(bin) <- gsub("1|2","", names(bin))
# 
# bin <- rbind(bin[,c(1,2,3,5)],
#              bin[,c(2,1,4,5)])
# 
# names(bin) <- c("player1","player2","win","ncontest")