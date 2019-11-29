#' Agreement between ranks
#' 
#' Summarise the concordance between two or more caracteristics from a baseline
#' ranking
#' 
#' @param baseline a rankings object of class 'rankings' or 'grouped_rankings' that will
#' be the baseline for comparing the other characteristics
#' @param compare a list of objects of same class and dimensions of \code{baseline} to be compared
#' @param labels a character to specify the name of compared chacteristics
#' @param x object of class 'gosset_agree' for the plotting method. Generates a 'ggplot' object that can be passed to any ggplot2 method
#' @param ... further arguments passed to methods. Not enabled yet
#' @return A data.frame with summary of agreement:
#' \item{labels}{the labels for each characteristic}
#' \item{kendall}{relative Kendall rank correlation coefficient}
#' \item{first}{relative agreement of a certain item being ranked first in the baseline}
#' \item{last}{relative agreement of a certain item being ranked last in the baseline}
#' @seealso \code{\link{kendallTau}} 
#' @examples 
#' # from the breadwheat data
#' # Compare the overall performance against 
#' # rankings on germination, grain quality and yield
#' 
#' data("breadwheat", package = "gosset")
#' 
#' R <- rank_tricot(data = breadwheat,
#'                  items = c(1:3),
#'                  input = c(18:19))
#' 
#' 
#' compare <- list()
#' 
#' compare[[1]] <- rank_tricot(data = breadwheat,
#'                             items = c("variety_a", "variety_b", "variety_c"),
#'                             input = c("germination_best","germination_worst"))
#' 
#' compare[[2]] <- rank_tricot(data = breadwheat,
#'                             items = c("variety_a", "variety_b", "variety_c"),
#'                             input = c("grainquality_best", "grainquality_worst"))
#' 
#' 
#' compare[[3]] <- rank_tricot(data = breadwheat,
#'                             items = c("variety_a", "variety_b", "variety_c"),
#'                             input = c("yield_best", "yield_worst"))
#' 
#' 
#' labels <- c("Germination", "Grain quality", "Yield")
#' 
#' 
#' agreement(R, 
#'           compare = compare, 
#'           labels = labels)
#'  
#' @export
agreement <- function(baseline, compare, labels = NULL){

  B <- baseline
  CC <- compare
  
  if(gosset:::.is_grouped_rankings(B)) {
    B <- B[1:length(B),, as.grouped_rankings = FALSE]
    
    CC <- lapply(CC, function(x) {
      x[1:length(x),,as.grouped_rankings = FALSE]
    })
    
  }
  
  
  if(gosset:::.is_rankings(B)) {
    
    B <- B[1:length(B),, as.rankings = FALSE]
    
    CC <- lapply(CC, function(x) {
      x[1:length(x),,as.rankings = FALSE]
    })
    
  }
  
  # number of rows 
  n <- nrow(B)
  
  
  # calculate Kendall correlation
  Kendall <- lapply(CC, function(x) {
    kendallTau(x, B)[[1]]
  })
  
  
  # take the first ranked item in B
  B_first <- apply(B, 1, function(x) {
    
    names(x)[which(x == 1)]
    
  })
  
  # take the last ranked item in B
  B_last <- apply(B, 1, function(x) {
    
    names(x)[which.max(x)]
    
  })
  
  
  # take the first ranked item in CC
  CC_first <- lapply(CC, function(y) {
    apply(y, 1, function(x) {
      
      names(x)[which(x == 1)]
      
    })
  })
  
  
  # take the first ranked item in CC
  CC_last <- lapply(CC, function(y) {
    apply(y, 1, function(x) {
      
      names(x)[which.max(x)]
      
    })
  })
  
  
  # compare first and last from B and CC
  first <- lapply(CC_first, function (x) {
    
    a <- x == B_first
    
    sum(a) / n
    
  })
  
  last <- lapply(CC_last, function (x) {
    
    a <- x == B_last
    
    sum(a) / n
    
  })
  
  if (is.null(labels)) {
    labels <- paste0("Characteristic ", 1:length(CC))
  }
  
  result <- tibble::tibble(labels = labels,
                           kendall = unlist(Kendall) * 100,
                           first = unlist(first) * 100,
                           last = unlist(last) * 100)
  
  class(result) <- c('gosset_agree', class(result))
  
  return(result)
 
}



# # @rdname agreement
# # @method plot gosset_agree
# # @export
# plot.gosset_agree <- function(x, ...) {
#   
#   x <- tidyr::gather(a, 
#                      key = "variable",
#                      value = "value",
#                      names(x)[2:ncol(x)])
#   
#   
#   
#   x$variable <- factor(x$variable, levels = c("kendall","first","last"))
#   
#   ggplot(x,
#          aes(y = x$value,
#              x = x$labels,
#              alpha = x$value)) +
#     geom_bar(stat = "identity",
#              position = "dodge",
#              col = alpha("gray50", 1), 
#              show.legend = FALSE) +
#     facet_wrap(. ~ variable) +
#     coord_flip() +
#     geom_hline(yintercept = 0:1, size = 1) +
#     scale_fill_manual(values = c("purple","forestgreen","red")) +
#     # geom_text(aes(y = agreement/2,
#     #               label = formattable::percent(agreement, 1)),
#     #           fontface = 2, size = 8, alpha = 1) +
#     scale_y_continuous(labels = scales::percent, 
#                        breaks = seq(0,1, by = 0.2),
#                        limits=c(0,1)) +
#     ylab("") + 
#     xlab("") +
#     theme(axis.text.y = element_text(size = 15,face = 2), 
#           strip.text = element_text(size = 15, face = 2))
#   
#   
#   
#   p <- 
#     ggplot2::ggplot(x) +
#     ggplot2::geom_bar(ggplot2::aes(y = x$victories, 
#                                    x = x$player2,
#                                    fill = x$victories), 
#                       stat = "identity", 
#                       col = "black") +
#     ggplot2::facet_wrap(. ~ x$player1,
#                         scales = "free_y", 
#                         ncol = 3) +
#     ggplot2::coord_flip() + 
#     ggplot2::scale_fill_gradient2(
#       limits = c(0, 1),
#       low =  scales::alpha("#FFFFFF", 0.90),
#       high =  scales::alpha("#0571B0", 0.90),
#       labels = scales::percent,
#       name = ""
#     ) +
#     ggplot2::scale_y_continuous(labels = scales::percent, 
#                                 limits = c(0, 1)) + 
#     labs(x = "Player", y = "Relative victories")
#   
#   return(p)
# }