# # S3 methods to plot data from gosset summarise functions
# 
# 
# # @rdname agreement
# # @method plot gosset_summ
# # @export
# plot.gosset_summ <- function(x, type = NULL, ...) {
#   
#   if (is.null(type)) {
#     NextMethod()
#   }
#   
#   
#   # force labels to be in the order as provided by input
#   labels_lv <- x$labels
#   
#   # force capital letter in type
#   names(x) <- c("labels","Kendall","First","Last")
#   
#   # put data in a long format
#   x <- split(x, x$labels)
#   x <- lapply(x, function(z) {
#     ag <- as.vector(t(z[2:4]))
#     ty <- as.vector(names(z[2:4]))
#     la <- unlist(rep(z[[1]], 3))
#     data.frame(labels = la, 
#                type = ty, 
#                agreement = ag,
#                stringsAsFactors = FALSE)
#   })
#   
#   x <- do.call("rbind", x)
#   
#   # convert type into factor 
#   x$type <- factor(x$type,
#                    levels = c("Kendall","First","Last"))
#   
#   # and labels into factor
#   x$labels <- factor(x$labels,
#                      levels = labels_lv)
#   
#   # plot
#   p <- 
#     ggplot2::ggplot(x,
#                     ggplot2::aes(
#                       y = x$agreement,
#                       x = x$labels,
#                       alpha = x$agreement,
#                       fill = x$type
#                     )) +
#     ggplot2::geom_bar(
#       stat = "identity",
#       position = "dodge",
#       col = "gray50",
#       show.legend = FALSE
#     ) +
#     ggplot2::facet_wrap(. ~ x$type) +
#     ggplot2::coord_flip() +
#     ggplot2::geom_text(
#       ggplot2::aes(y = x$agreement / 2,
#                    label = round(x$agreement, 0)),
#       fontface = 2,
#       size = 8,
#       alpha = 1
#     ) +
#     ggplot2::scale_y_continuous(
#       labels = paste0(seq(0, 100, by = 25), "%"),
#       breaks = seq(0, 100, by = 25),
#       limits = c(0, 100)
#     ) +
#     ggplot2::theme(
#       axis.text.y = ggplot2::element_text(size = 15, face = 2),
#       strip.text = ggplot2::element_text(size = 15, face = 2)
#     ) +
#     ggplot2::labs(x = "", y = "")
#   
#   return(p)
# }