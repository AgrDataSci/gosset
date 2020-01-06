#' Temperature indices
#'
#' Compute temperature indices over a timespan 
#'
#' @param object a numeric vector of geographic coordinates (lonlat) or
#' an array with two dimensions containing the temperature data;
#' 1st dimension contains the day temperature and 2nd dimension the night
#' temperature. When lonlat is used, the function makes a call to
#' \code{nasapower::get_power()} to fetch and concatenate environmental
#' data from NASA POWER (\url{https://power.larc.nasa.gov/})
#' @param day.one a vector of class 'Date' for the starting date to 
#' capture the environmental data
#' @param span an integer or a vector with integers for the duration 
#' of the timespan to be captured
#' @param ... additional arguments passed to methods
#' @return A dataframe with temperature indices:
#' \item{maxDT}{maximun day temperature (degree Celsius)}
#' \item{minDT}{minimum day temperature (degree Celsius)}
#' \item{maxNT}{maximun night temperature (degree Celsius)}
#' \item{minNT}{minimum night temperature (degree Celsius) }
#' \item{DTR}{diurnal temperature range (mean difference between DT 
#' and NT (degree Celsius)) }
#' \item{SU}{summer days, number of days with maximum temperature > 
#' 30 (degree Celsius)}
#' \item{TR}{tropical nights, number of nights with maximum 
#' temperature > 25 (degree Celsius) }
#' @family climatology functions
#' @references 
#' Aguilar E., et al. (2005). Journal of Geophysical Research, 
#' 110(D23), D23107. https://doi.org/10.1029/2005JD006119.
#' 
#' Kehel Z., et al. (2016). Identifying Climate Patterns during 
#' the Crop-Growing Cycle from 30 Years of CIMMYT Elite Spring 
#' Wheat International Yield Trials. In: Applied Mathematics and 
#' Omics to Assess Crop Genetic Resources for Climate Change Adaptive 
#' Traits (eds Bari A., Damania A. B., Mackay M., Dayanandan S.), 
#' pp. 151–174. CRC Press.
#' 
#' Sparks A. H. (2018). Journal of Open Source Software, 3(30), 1035. 
#' https://doi.org/10.21105/joss.01035.
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
#' # get temperature indices for the first 40 days
#' temperature(lonlat,
#'             day.one = pdates,
#'             span = 40)
#' }
#' @export
temperature <- function(object, day.one = NULL, span = 150,
                        ...)
{
  
  index <- c("maxDT","minDT","maxNT","minNT","SU","TR","DTR")
  
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
    night <- .get_timeseries(object[, , 2], day.one, span, ...)
  }
  
  n <- nrow(day)
  
  ind <- tibble::as_tibble(matrix(nrow = n, 
                                  ncol = length(index), 
                                  dimnames = list(1:n, index)))
  
  # maximun day temperature (degree Celsius)
  if ("maxDT" %in% index) {
    ind["maxDT"] <- apply(day, 1, function(x) {
      max(x, na.rm = TRUE)
    })
  }
  # minimum day temperature (degree Celsius)
  if ("minDT" %in% index) {
    ind["minDT"] <- apply(day, 1, function(x) {
      min(x, na.rm = TRUE)
    })
  }
  # maximum night temperature (degree Celsius)
  if ("maxNT" %in% index) {
    ind["maxNT"] <-
      apply(night, 1, function(x) {
        max(x, na.rm = TRUE)
      })
  }
  # minimum night temperature (degree Celsius)
  if ("minNT" %in% index) {
    ind["minNT"] <-
      apply(night, 1, function(x) {
        min(x, na.rm = TRUE)
      })
  }
  # diurnal temperature range, mean difference between DT and NT (degree Celsius)
  if ("DTR" %in% index) {
    ind["DTR"] <-
      apply((day - night), 1, function(x) {
        mean(x, na.rm = TRUE)
      })
  }
  # summer days, number of days with maximum temperature > 30 C
  if ("SU" %in% index) {
    ind["SU"] <- apply(day, 1, function(x) {
      sum(x > 30, na.rm = TRUE)
    })
  }
  # tropical nights, number of nights with maximum temperature > 25 C
  if ("TR" %in% index) {
    ind["TR"] <-
      apply(night, 1, function(x) {
        sum(x > 25, na.rm = TRUE)
      })
  }
  
  return(ind)
  
}
