#' Compare agreement between two methods
#' 
#' Measures the agreement between two methods
#'  
#' @param x a numeric vector, or an object  
#'  of class \code{PlackettLuce}
#' @param y a numeric vector, or an object  
#'  of class \code{PlackettLuce}
#' @param labels optional, a vector with the same length \var{x}
#'  to plot values
#' @param ... additional arguments passed to methods
#' @return a ggplot with the agreement
#' @examples
#' set.seed(1)
#' x <- runif(10, -1, 2)
#' 
#' set.seed(2)
#' y <- runif(10, -1, 2)
#' 
#' compare(x, y)
#' 
#' @references 
#' Bland, M. J., and Altman, D. G. (1986).
#'  Lancet (8476):307-10.
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @importFrom ggplot2 geom_hline geom_point
#' @importFrom stats coefficients sd
#' @importFrom ggrepel geom_text_repel
#' @export
compare <- function(x, y, ...){
  
  UseMethod("compare")
  
}

#' @rdname compare
#' @export
compare.default <- function(x, y, labels = NULL, ...){
  
  if (is.null(labels)) {
    labels <- LETTERS[1:length(x)]
  }
  
  comp <- data.frame(diff = x - y,
                     aver = rowMeans(cbind(x, y)),
                     item = labels)
  
  # the mean of the difference
  d <- mean(comp$diff)
  
  # the standard deviation of the difference
  s <- stats::sd(comp$diff)
  
  # limits of agreement
  llim <- d - (2 * s)
  ulim <- d + (2 * s)
  
  aver <- comp$aver
  item <- comp$item
  
  diff_plot <- 
    ggplot2::ggplot(comp,
                    ggplot2::aes(x = aver, y = diff, label = item)) +
    ggplot2::geom_hline(yintercept = llim, linetype = "dashed", col = "grey30") +
    ggplot2::geom_hline(yintercept = ulim, linetype = "dashed", col = "grey30") +
    ggplot2::geom_hline(yintercept = mean(comp$diff), col = "red") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggrepel::geom_text_repel() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
          text = ggplot2::element_text(size = 15),
          axis.text = ggplot2::element_text(size = 13, color = "grey20", face = 2),
          axis.title = ggplot2::element_text(size = 12, color = "grey20", face = 2)) +
    ggplot2::labs(x = "Average", 
         y = "Difference (x - y)")
  
  return(diff_plot)
  
}

#' @rdname compare
#' @method compare PlackettLuce
#' @export
compare.PlackettLuce <- function(x, y, ...){
  
  # get the probability of winning and than set back as log
  X <- log(stats::coefficients(x, log = FALSE))
  
  Y <- log(stats::coefficients(y, log = FALSE))
  
  lab <- names(X)
  
  compare(x = X, y = Y, labels = lab)
  
}
