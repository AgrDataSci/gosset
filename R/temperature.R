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
#' capture the environmental data
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
#' temperature > 25 (degree Celsius)}
#' \item{CFD}{consecutive frosty days, number of days with temperature 
#' bellow 0 degree Celsius}
#' 
#'  When \var{timeseries} = \code{TRUE}, an id is created, 
#'  which is the index for the rownames of the provided \var{object}.
#' 
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
#' # random points within bbox(11, 12, 55, 58)
#' set.seed(123)
#' lonlat <- data.frame(lon = runif(3, 11, 12),
#'                      lat = runif(3, 55, 58))
#' 
#' # random dates within 2018-05-15 and 2018-05-2
#' set.seed(321)
#' pdates <- as.integer(runif(3, 17666, 17670))
#' pdates <- as.Date(pdates, origin = "1970-01-01")
#' 
#' # get temperature indices for 40 days after day.one
#' temperature(lonlat,
#'             day.one = pdates,
#'             span = 40)
#' 
#' # get temperature indices over a time series
#' temperature(lonlat,
#'             day.one = pdates,
#'             span = 40,
#'             timeseries = TRUE,
#'             intervals = 5)
#' }
#' @export
temperature <- function(object, day.one = NULL, 
                        span = 150, timeseries = FALSE,
                        intervals = 5,
                        ...)
{
  
  index <- c("maxDT","minDT","maxNT","minNT","DTR","SU","TR","CFD")
  
  # get timespan for the day temperature
  if (dim(object)[2] == 2) {
    day <- .get_timeseries(object = object, 
                           day.one = day.one, 
                           span = span, 
                           pars = "T2M_MAX",
                           ...)
  } else {
    day <- .get_timeseries(object = object[ , ,1], 
                           day.one = day.one, 
                           span = span)
  }
  
  # get timespan for the night temperature
  if (dim(object)[2] == 2) {
    night <- .get_timeseries(object = object, 
                             day.one = day.one, 
                             span = span, 
                             pars = "T2M_MIN",
                             ...)
  } else {
    night <- .get_timeseries(object = object[ , ,2], 
                             day.one = day.one, 
                             span = span)
  }
  
  n <- nrow(day)
  
  if (timeseries) {
    
    # it might happen that when bins are not well distributed across dates
    # in that case the last values are dropped
    # for example, divide the periods of 7 days in a time series of 53 days
    # in that case, the last four observations are dropped to fit in a vector of
    # length == 49 (the maximum integer from dividing days/intervals)
    # organise bins, ids and dates
    bins <- floor(ncol(day)/intervals)
    
    bins <- rep(1:bins, each = intervals, length.out = NA)
    
    # ids are the row names in r
    ids <- rownames(day)
    
    # dates are the first day for each bin
    dates <- NULL
    # for diffent day.one a loop is required to take the
    # sequence of days and the first day in each bin
    for (i in seq_along(day.one)) {
      
      d <- day.one[[i]]:(day.one[[i]] + (span - 1))
      
      d <- d[1:length(bins)]
      
      d <- d[!duplicated(bins)]
      
      d <- rep(d, each = length(index))
      
      dates <- c(dates, d)
      
      
    }
    
    dates <- as.Date(dates, origin = "1970-01-01")
    
    # transpose and keep values until the end of bins
    day <- t(day)
    
    night <- t(night)
    
    # keep data within the lenght of bins
    day <- as.data.frame(day[1:length(bins), ])
    
    night <- as.data.frame(night[1:length(bins), ])
    
    # split by bins
    day <- split(day, bins)
    
    night <- split(night, bins)
    
    ind <- mapply(function(X, Y) {
      
      Z <- rbind(X, Y)
      
      i <- apply(Z, 2, function(z) {
        l <- length(z)
        
        XX <- z[1:(l / 2)]
        
        YY <- z[((l / 2) + 1):l]
        
        c(maxDT = .max_temperature(XX),
          minDT = .min_temperature(XX),
          maxNT = .max_temperature(YY),
          minNT = .min_temperature(YY),
          DTR   = .temperature_range(XX, YY),
          SU    = .summer_days(XX),
          TR    = .tropical_nights(YY),
          CFD   = .frosty_days(YY))
        
      })
      
      i <- data.frame(id    = rep(ids, each = length(index)),
                      index = rep(index, times = length(ids)),
                      value = as.vector(unlist(i)), 
                      stringsAsFactors = FALSE)
      
    }, X = day, Y = night, SIMPLIFY = FALSE)
    
    ind <- do.call("rbind", ind)
    
    ind$id <- as.integer(ind$id)
    
    ind$index <- as.character(ind$index)
    
    ind <- ind[order(ind$id), ]
    
    ind$date <- dates
    
    ind <- tibble::as_tibble(ind)
    
    ind <- ind[, c("id", "date", "index", "value")]
    
  } 
  
  
  if (!timeseries) {
    
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
                      TR    = .tropical_nights(y),
                      CFD   = .frosty_days(y))
      
    }, X = day, Y = night)
    
    ind <- matrix(unlist(ind), 
                  nrow = n, 
                  ncol = length(index), 
                  byrow = TRUE)
    
    dimnames(ind)[[2]] <- index
    
    ind <- tibble::as_tibble(ind)
    
    
    
  }
  
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

#' Consecutive frosty days 
#' 
#' Compute number of consecutive days with temperature 
#' bellow 0 degree Celsius
#' 
#' @param x numeric vector
#' @return the CFD index
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' .frosty_days(r)
#' @noRd
.frosty_days <- function(x)
{
  # the function rle is applied
  # which looks for the sequencies of numbers
  # in this case, zeros (0)
  # take the maximum sequency
  # first all values <= 0 are converted to zero (0)
  fd <- ifelse(x <= 0, 0, x)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(fd)$values
  
  keep <- keep == 0
  
  fd <- rle(fd)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  # which means there is no frosty days
  if (length(fd) == 0) {
    fd <- 0
  }
  
  # if there is values, take the maximum sequency
  if (length(fd) != 0) {
    fd <- max(fd, na.rm = TRUE)
  }
  
  return(fd)
  
}