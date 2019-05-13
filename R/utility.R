#' Whole number
#'
#' Find integer values in a vector with decimals
#'
#' @param x vector with numbers of class "numeric" or "integer"
#' @return a logical vector 
#' @examples
#' 
#' x <- c(1, 2, 1.4, 3, 4.01)
#' 
#' is_wholenumber(x)
#'
#' @export
is_wholenumber <- function(x) {
  x <- as.vector(t(x))
  x <- x %% 1 == 0
  return(x)
}

#' Decimal number
#'
#' Find a decimal number in a vector with integers
#'
#' @param x vector with numbers of class "numeric" or "integer"
#' @return a logical vector 
#' @examples
#' 
#' x <- c(1, 2, 1.4, 3, 4.01)
#' 
#' is_decimal(x)
#' 
#' @export
is_decimal <- function(x) {
  x <- as.vector(t(x))
  x <- !x %% 1 == 0
  return(x)
}

#' Numbers into rankings
#'
#' Group and convert numeric values into integer ranks
#'
#' @param object vector with numbers of class "numeric" or "integer"
#' @param id optional, a vector with ids to group values
#' @param ... additional arguments passed to methods
#' @return a data frame with ranked values where highest values are placed first
#' @examples
#' 
#' # passing a vector with decimals
#' # the highest positive value is the best scored item
#' # negative values are included as least scored items
#' 
#' x <- c(1, 2, 1.4, 3, 4.01, -0.5)
#' 
#' num2rank(x)
#' 
#' # passing a vector with an id
#' # ids are used to group values and 
#' # return a rank for each group
#' 
#' x <- c(1, 2, 1.4, 3, 4.01, -0.5)
#' id <- c(rep(1, 3), rep(2, 3))
#' 
#' num2rank(x, id = id)
#' 
#' @export
num2rank <- function(object, id = NULL, ...){
  
  dots <- list(...)
  
  bindwith <- dots[["bindwith"]]
  
  isdf <- "data.frame" %in% class(object)
  
  if (!isdf) {
    object <- as.data.frame(object)
    names(object) <- "rank"
  }
  
  if (!is.null(bindwith)) {
    object <- cbind(object, bindwith)
  }
  
  if(is.null(id)) {
    id <- rep(1, nrow(object))
  }

  object <- cbind(id = id, object)
  
  object <- dplyr::mutate(dplyr::group_by(object, id),
                          rank = rank((rank - 1) * -1, na.last = "keep"))
  
  
  object <- tibble::as_tibble(object) 
  
  return(object)
  
}




