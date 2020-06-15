#' Favourability scores
#'
#' Summarise favourability scores from permutation contests
#' 
#' @author Nicolas Greliche, Sam Dumble and Kauê de Sousa
#' @family summarise functions
#' @aliases favorite
#' @aliases favourite
#' @aliases summarise_favorite
#' @param object an object of class \code{rankings} or \code{grouped_rankings}
#' @param x an object of class \code{gosset_fvrt} for the plotting method. 
#' Generates a \code{ggplot} object that can be passed to any \pkg{ggplot2} method
#' @param ... additional arguments passed to methods. See details
#' @details  
#' \code{minlength} an integer, passed to \code{abbreviate()} to define the
#'  minimum length of the abbreviations
#' @return A data.frame with the descriptive statistics:
#' \item{N}{number of times the given item was evaluated}
#' \item{best}{relative number of times (in percentage) the given item was
#'  ranked as first}
#' \item{worst}{relative number of times (in percentage) the given item was 
#' ranked as worst}
#' \item{wins}{relative number of times (in percentage) the given item wins 
#' against the others}
#' \item{fav_score}{the favourability score, which is the difference between 
#' best and worst performance}
#' @examples
#' data("breadwheat", package = "gosset")
#' 
#' R <- rank_tricot(data = breadwheat,
#'                  items = c("variety_a", "variety_b", "variety_c"),
#'                  input = c("overall_best", "overall_worst"))
#' 
#' fav <- summarise_favourite(R)
#' 
#' plot(fav)
#' 
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @importFrom ggplot2 ggplot aes geom_hline geom_bar coord_flip scale_y_continuous 
#'  scale_fill_gradient2 labs
#' @importFrom tibble tibble
#' @export
summarise_favourite <- function(object, ...){
  
  if (.is_grouped_rankings(object)) {
    dataR <- object[1:length(object), , as.grouped_rankings = FALSE]
  }
  
  if (.is_rankings(object)) {
    dataR <- object[1:length(object), , as.rankings = FALSE]
  }
  
  # get names of tested items
  itemnames <- dimnames(dataR)[[2]]
  n <- dim(dataR)[[1]]
  
  # items ranked as first (best)
  firstR <- apply(dataR, 1, function(x){
    x <- x[x != 0]
    names(x)[which.min(x)]
  })
  
  firstR <- table(firstR)
  
  # item ranked as last (worst)
  lastR <- apply(dataR, 1, function(x){
    x <- x[x != 0]
    names(x)[which.max(x)]
  })
  
  lastR <- table(lastR)

  dataR <- as.data.frame(as.vector(unlist(dataR)))
  dataR$items <- rep(itemnames, each = n)
  dataR <- dataR[dataR[,1] > 0, ]
  dataR <- table(dataR$items)
  
  dataR <- as.data.frame(dataR,
                         stringsAsFactors = FALSE)
  
  rownames(dataR) <- dataR[, "Var1"]
  
  dataR$first <- 0
  dataR[names(firstR), "first"] <- firstR
  
  dataR$last <- 0
  dataR[names(lastR), "last"] <- lastR
  
  dataR$best <- (dataR$first / dataR$Freq) * 100
  
  dataR$worst <- (dataR$last / dataR$Freq) * 100
  
  # times it wins
  wins <- 100 - dataR$worst
    
    
  # favourability score
  fav_score <- dataR$best - dataR$worst
  
  sumstats <- tibble::tibble(items = dataR$Var1,
                             N = dataR$Freq,
                             best =  dataR$best,
                             worst = dataR$worst,
                             wins = wins,
                             fav_score = fav_score)
  

  sumstats <- sumstats[rev(order(sumstats$fav_score)), ]
  
  class(sumstats) <- c("gosset_fvrt", class(sumstats))
  
  return(sumstats)

}

#' @inheritParams summarise_favourite
#' @export
summarise_favorite <- function(...){
  
  summarise_favourite(...)
  
}

#' @rdname summarise_favourite
#' @method plot gosset_fvrt
#' @export
plot.gosset_fvrt <- function(x, ...) {
  
  # check for large characters and reduce number of characters
  # creating abbreviantions
  x$items <- .reduce(x$items, ...)
  
  # get order of players based on their performance
  player_levels <- rev(.player_order(x, "items", "fav_score"))
  
  x$items <- factor(x$items, levels = player_levels)
  
  fav_score <- x$fav_score
  items <- x$items
  
  p <- ggplot2::ggplot(data = x, 
                       ggplot2::aes(y = fav_score, 
                                    fill = fav_score, 
                                    x = items)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_bar(stat = "identity", 
                      col = "#000000",
                      width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(breaks = seq(-100, 100, by = 20), 
                                limits = c(-100, 100)) +
    ggplot2::scale_fill_gradient2(name = "Favourability",
                                  low = "#CA0020",
                                  mid = "#f7f7f7",
                                  high = "#0571B0",
                                  limits = c(-100, 100)) +
    ggplot2::labs(x = "Items", y = "")
  
  return(p)
}
