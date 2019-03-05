# check is number is a whole number (no decimals)
#' @export
is_wholenumber <- function(x){
  x <- as.vector(t(x))
  x <- x%%1==0
  return(x)
}

# check is number is not whole number (has decimals)
#' @export
is_decimal <- function(x){
  x <- as.vector(t(x))
  x <- !x%%1==0
  return(x)
}

# group and convert numeric values into integer ranks
# the highest positive value is the best scored item
# negative values are included as least scored items
#' @export
num2rank <- function(object){
  object <- dplyr::mutate(dplyr::group_by(object , id),
                          rank = rank((rank -1)*-1, 
                                      na.last = "keep"))
  
  return(object)
  
}

# Testing a grouped_rankings object
#' @export
is_grouped_rankings <- function(object) {
  
  return(class(object) == "grouped_rankings")
  
}

# Testing a paircomp object
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