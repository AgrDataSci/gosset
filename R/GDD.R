#' Growing degree-days
#' 
#' Compute number of days required to reach Growing degree-days.
#' Is a heuristic tool in phenology. Growing degree-days are a measure of
#' heat accumulation used to predict plant and animal development rates.
#' Growing degree-days are calculated by taking the integral of warmth above 
#' a base temperature.
#' 
#' @param degree.days an integer for the degree days required by the crop 
#' (look at the physiology of the focal crop)
#' @param base an integer for the base temperature.
#' @inheritParams temperature
#' @return The number of days required to reach the growing degree days.
#' @family climatology functions
#' @references 
#' Prentice I. C., et al. (1992) Journal of Biogeography, 19(2), 117. 
#' https://doi.org/10.2307/2845499.
#' 
#' @examples
#' \donttest{
#' 
#' library("gosset")
#' library("nasapower")
#' 
#' # random geographic locations around bbox(11, 12, 55, 58)
#' lonlat <- data.frame(lon = runif(10, 11, 12),
#'                      lat = runif(10, 55, 58))
#' 
#' # random planting dates around 2018-05-15 and 2018-05-20
#' pdates <- as.integer(runif(10, 17666, 17670))
#' pdates <- as.Date(pdates, origin = "1970-01-01")
#' 
#' # Calculate the days required for the plants in these plots to reach the
#' # maturity. The crop requires ~1800 degree days for it.
#' 
#' GDD(lonlat, 
#'     day.one = pdates,
#'     degree.days = 1800,
#'     base = 5)
#'}
#'
#' @importFrom tibble tibble
#' @export
GDD <- function(object, day.one = NULL, degree.days = NULL,
                base = 10, span = 150, ...)
{
  
  # validate parameters
  if (is.null(degree.days)) {
    stop("degree.days is missing with no default \n")
  }
  

  # get timespan for the day temperature
  if (dim(object)[2] == 2) {
    day <- .get_timeseries(object, day.one, span, pars = "T2M_MAX", ...)
  } else {
    day <- .get_timeseries(object[, , 1], day.one, span, ...)
  }
  
  # get timespan for the night temperature
  if (dim(object)[2] == 2) {
    night <- .get_timeseries(object, day.one, span, pars = "T2M_MIN", ...)
  } else {
    night <- .get_timeseries(object[,,2], day.one, span, ...)
  }
  
  # get the difference between day and night temperature
  Y <- (((day + night) / 2) - base)
  
  # sum temperature values until reach the defined degree days
  Y <- apply(Y, 1, function(x){
    
    for (d in 1:length(x)) {
      
      i <- d
      
      if (sum(x[1:d]) > degree.days) break}
    
    return(i)
  })
  
  return(tibble::tibble(GDD = Y))
}

