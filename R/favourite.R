#' Favourability scores
#'
#' Summarise favourability scores from tricot data
#' 
#' @inheritParams rank_tricot
#' @param reorder logical, if items should be reordered from higher favourability score to least favourability score
#' @param x an object of class 'gosset_fvrt' for the plotting method. Generates a 'ggplot' object that can be passed to any ggplot2 method
#' @aliases favorite
#' @return A data.frame with the descriptive statistics:
#' \item{N}{number of times the given item was evaluated}
#' \item{best}{relative number of times (in percentage) the given item was ranked as first}
#' \item{worst}{relative number of times (in percentage) the given item was ranked as worst}
#' \item{wins}{relative number of times (in percentage) the given item wins against the others}
#' \item{fav_score}{the favourability score, which is the difference between best and worst performance}
#' @examples
#' data("breadwheat", package = "gosset")
#' 
#' fv <- favourite(data = breadwheat,
#'                 items = c(1:3),
#'                 input = c(18:19))
#' 
#'  
#' @import ggplot2
#' @export
favourite <- function(data = NULL, items = NULL, 
                      input = NULL, reorder = TRUE){
  
  # check data
  args <- .check_data(data = data,
                      items = items, 
                      input = input)
  
  # take decoded rankings
  args[["full.output"]] <- TRUE
  
  dataR <- do.call("rank_tricot", args)  

  dataR <- dataR[[3]]
  
  # get names of tested items
  itemnames <- sort(unique(as.vector(dataR)))
  
  # items ranked as first (best)
  firstR <- dataR[, 1]
  
  # item ranked as last (worst)
  lastR <- dataR[,ncol(dataR)]
  
  
  # run over items to check the number of times it is ranked
  # as first and last 
  inrow <- NULL
  wins <- NULL
  losses <- NULL
  
  # get data.frame with randomised items
  X <- args$data[, args$items]
  
  for (i in seq_along(itemnames)) {
    # check the row where item i is present
    inrow_i <- apply(X, 1, function(x) {
      y <- any(x == itemnames[i])
      as.integer(y)
    })
    
    # put it into a single matrix
    inrow <- cbind(inrow, inrow_i)
    
    # check where item i is ranked as first (best)
    wins <- cbind(wins, ifelse(firstR == itemnames[i], 1, 0))
    
    # check where item i is ranked as last (worst)
    losses <- cbind(losses, ifelse(lastR == itemnames[i], 1, 0))
    
  }
  
  # name matrixes
  colnames(inrow)  <- paste("n", 1:(length(itemnames)), sep = "")
  colnames(wins)   <- paste("b", 1:(length(itemnames)), sep = "")
  colnames(losses) <- paste("w", 1:(length(itemnames)), sep = "")
  
  # compute
  # best performance
  best_per <- 100 * colSums(wins) / colSums(inrow)
  
  # worst performance
  worst_per <- 100 * colSums(losses) / colSums(inrow)
  
  # times it wins
  wins <- ((2 * colSums(wins)) + colSums(inrow - wins - losses)) / (2 * colSums(inrow))
  
  # favourability score
  fav_score <- best_per - worst_per
  
  sumstats <- tibble::tibble(items = itemnames,
                             N = colSums(inrow),
                             best =  best_per,
                             worst = worst_per,
                             wins = wins,
                             fav_score = fav_score)
  

  if (reorder) {
    sumstats <- sumstats[rev(order(sumstats$fav_score)), ]
  }
  
  class(sumstats) <- c("gosset_fvrt", class(sumstats))
  
  return(sumstats)

}

#' @inheritParams favourite
#' @export
favorite <- function(...){
  
  favourite(...)
  
}

#' @rdname favourite
#' @method plot gosset_fvrt
#' @export
plot.gosset_fvrt <- function(x, ...) {
  
  p <- ggplot2::ggplot(data = x, 
                       ggplot2::aes(y = x$fav_score, 
                                    fill = x$fav_score, 
                                    x = x$items))+
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::geom_bar(stat = "identity", col = "black")+
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(breaks = seq(-100, 100, by = 20))+
    ggplot2::scale_fill_gradient2(name = "Favourability",
                                  low = "#CA0020",
                                  mid = "#FFFFFF",
                                  high = "#0571B0",
                                  limits = c(-100, 100)) +
    labs(x = "Items", y = "")
  
  return(p)
}