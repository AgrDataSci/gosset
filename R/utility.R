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
is_wholenumber <- function(x){
  x <- as.vector(t(x))
  x <- x%%1==0
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
is_decimal <- function(x){
  x <- as.vector(t(x))
  x <- !x%%1==0
  return(x)
}

#' Numbers into rankings
#'
#' Group and convert numeric values into integer ranks
#'
#' @param x vector with numbers of class "numeric" or "integer"
#' @return a vector with ranked values where highest values are placed first
#' @examples
#' 
#' x <- c(1, 2, 1.4, 3, 4.01)
#' 
#' is_wholenumber(x)
#' 
# the highest positive value is the best scored item
# negative values are included as least scored items
#' @export
num2rank <- function(object, ...){

  # dots <- list(...)
  #   
  # id <- dots[["id"]]
  # 
  # if(is.null(id)) {
  #   id <- rep(1, length(object))
  # }
  # 
  # object$id <- id
  
  object <- dplyr::mutate(dplyr::group_by(object , id),
                          rank = rank((rank -1)*-1, 
                                      na.last = "keep"))
  
  return(object)
  
}

# Test a grouped_rankings object
#' @export
is_grouped_rankings <- function(object) {
  
  return(class(object) == "grouped_rankings")
  
}

# Test a paircomp object
#' @export
is_paircomp <- function(object) {
  
  return(class(object) == "paircomp")
  
}

# logical function for > greater 
#' @export
is_greater <- function(x, y) {
  x > y
}


# logical function for < lower
#' @export
is_lower <- function(x, y) {
  x < y
}

# round to the nearest 5
#' @export
round5 <- function(x, base.value) {
  
  base.value * round( x / base.value )
  
}