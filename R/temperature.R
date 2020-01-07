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
#' @param day.one a vector of class \code{Date} for the starting date to 
#' capture the environmental data.
#' @param span an integer or a vector with integers for the duration 
#' of the timespan to be captured
#' @param timeseries logical, \code{FALSE} for a single point time series
#'  observation or \code{TRUE} for a time series based on \var{intervals}
#' @param intervals integer no lower than 5, for the days intervals when
#'  \var{timeseries} = \code{TRUE}
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
#' 110(D23), D23107. \cr\url{https://doi.org/10.1029/2005JD006119}
#' 
#' Kehel Z., et al. (2016). Identifying Climate Patterns during 
#' the Crop-Growing Cycle from 30 Years of CIMMYT Elite Spring 
#' Wheat International Yield Trials. In: Applied Mathematics and 
#' Omics to Assess Crop Genetic Resources for Climate Change Adaptive 
#' Traits (eds Bari A., Damania A. B., Mackay M., Dayanandan S.), 
#' pp. 151–174. CRC Press.
#' 
#' Sparks A. H. (2018). Journal of Open Source Software, 3(30), 1035. 
#' \cr\url{https://doi.org/10.21105/joss.01035}
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
temperature <- function(object, day.one = NULL, 
                        span = 150, timeseries = FALSE,
                        intervals = 5,
                        ...)
{
  index <- c("maxDT","minDT","maxNT","minNT","DTR","SU","TR")
  
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
  
  day <- split(day, 1:n)
  
  night <- split(night, 1:n)
  
  ind <- mapply(function(X, Y) {
    
    x <- as.vector(as.matrix(X))
    y <- as.vector(as.matrix(Y))
    
    x <- data.frame(maxDT = .max_temperature(x),
                    minDT = .min_temperature(x),
                    maxNT = .max_temperature(y),
                    minNT = .min_temperature(y),
                    DTR   = .temperature_range(x, y),
                    SU    = .summer_days(x),
                    TR    = .tropical_nights(y))
    
  }, X = day, Y = night)
  
  ind <- matrix(unlist(ind), 
                nrow = n, 
                ncol = length(index), 
                byrow = TRUE)
  
  ind <- tibble::as_tibble(ind)
  
  names(ind) <- index
  
  return(ind)
  
}


#' Maximum temperature
#' 
#' @param x a numeric vector
#' @return the maximum temperature 
#' @examples 
#' set.seed(123)
#' temp <- runif(10, 25, 34)
#' .max_temperature(day)
#' @noRd
.max_temperature <- function(x) {
  max(x, na.rm = TRUE)
}

#' Minimum temperature
#'  
#' @param x a numeric vector
#' @return the minimum temperature 
#' @examples 
#' set.seed(123)
#' temp <- runif(10, 25, 34)
#' .min_temperature(day)
#' @noRd
.min_temperature <- function(x) {
  min(x, na.rm = TRUE)
}

#' Diurnal temperature range
#' 
#' Compute mean mean difference between day and night
#' temperature (degree Celsius)
#' 
#' @param x a numeric vector
#' @param y a numeric vector
#' @return the diurnal temperature range 
#' @examples 
#' set.seed(123)
#' day <- runif(10, 25, 34)
#' 
#' set.seed(321)
#' night <- runif(10, 21, 30)
#' 
#' .temperature_range(day, night)
#' @noRd
.temperature_range <- function(x, y) {
  dtr <- x - y
  mean(dtr, na.rm = TRUE)
}

#' Summer days
#' 
#' Compute number of days with maximum temperature > 30 C
#' 
#' @param x a numeric vector
#' @return the summer days index
#' @examples 
#' x <- c(30, NA, 31, 32, 34)
#' .summer_days(x)
#' @noRd
.summer_days <- function(x) {
  x <- x > 30
  sum(x, na.rm = TRUE)
}

#' Tropical days
#' 
#' Compute number of nights with maximum temperature > 25 C
#' 
#' @param x a numeric vector
#' @return the summer days index
#' @examples 
#' x <- c(22, NA, 23, 26, 27, 21)
#' .tropical_nights(x)
#' @noRd
.tropical_nights <- function(x) {
  x <- x > 25
  sum(x, na.rm = TRUE)
}


