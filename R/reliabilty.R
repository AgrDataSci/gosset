#' Reliability based on worth parameters
#' 
#' Measures the precision of estimated values, and
#'  the potential response to selection on those 
#'  estimated values
#'  
#' @param x a numeric vector, or an object of 
#'  of class \code{PlackettLuce} or \code{pltree}
#' @param y numeric, the reference value
#' @param ref a character or integer for indexing the
#'  element containing reference values in \var{x}
#' @param ... additional arguments passed to methods
#' @return the reliability based on the worth parameters
#' @examples
#' # vector example
#' x <- c(9.5, 12, 12.3, 15)
#' y <- 11.2
#' reliability(x, y)
#'  
#' # PlackettLuce example
#' library("PlackettLuce") 
#' 
#' R <- matrix(c(1, 2, 4, 3,
#'               4, 1, 2, 3,
#'               2, 3, 1, 4,
#'               4, 2, 3, 1,
#'               2, 1, 4, 3,
#'               1, 4, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' 
#' mod <- PlackettLuce(R)
#' 
#' reliability(mod, ref = "apple")
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @export
reliability <- function(x, ...) {
    
  UseMethod("reliability")
  
}

#' @rdname reliability
#' @export
reliability.default <- function(x, y = NULL, ...) {
  
  dots <- list(...)

  ref <- dots[["ref"]]
  
  worth <- x
  
  if (is.null(ref)) {
    ref_worth <- y
  }
  
  if (is.null(y)) {
    ref_worth <- worth[ref]
  }
  
  1 - ((ref_worth - worth) / worth)
  
}

#' @rdname reliability
#' @method reliability PlackettLuce
#' @export
reliability.PlackettLuce <- function(x, ref, ...) {
  
  worth <- coef(x, log = FALSE) 
  
  reliability(x = worth, y = NULL, ref = ref)

}

#' @rdname reliability
#' @method reliability pltree
#' @export 
reliability.pltree <- function(x, ref, ...) {
  
  worth <- coef(x, log = FALSE) 
  
  t(apply(worth, 1, function(z){
    reliability(x = z, y = NULL, ref = ref)
  }))
  
}
