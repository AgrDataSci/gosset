#' Temperature indices
#'
#' Compute temperature indices over a timespan 
#'
#' @param object a numeric vector of geographic coordinates (lonlat) or
#' an array with two dimensions containing the temperature data;
#' 1st dimension contains the day temperature and 2nd dimension the night temperature.
#' When lonlat is used, the function makes a call to
#' \code{nasapower::get_power} to fetch and concatenate environmental data from NASA POWER
#' (\url{https://power.larc.nasa.gov/})
#' @param day.one a vector of class 'Date' for the starting date to capture the environmental data
#' @param span an integer or a vector with integers for the duration of the timespan to be captured
#' @param index optional, a character or a vector with characters for the indices to be computed. 
#' All indices are given by default
#' @param ... additional arguments passed to methods
#' @return A dataframe with selected indices. Options are:
#' \item{maxDT}{maximun day temperature (degree Celsius)}
#' \item{minDT}{minimum day temperature (degree Celsius)}
#' \item{maxNT}{maximun night temperature (degree Celsius)}
#' \item{minNT}{minimum night temperature (degree Celsius) }
#' \item{DTR}{diurnal temperature range (mean difference between DT and NT (degree Celsius)) }
#' \item{SU}{summer days, number of days with maximum temperature > 30 (degree Celsius)}
#' \item{TR}{tropical nights, number of nights with maximum temperature > 25 (degree Celsius) }
#' @examples
#' 
#' # Compute indices using temperature data from MODIS MYD11A1
#' data("breadwheat_modis", package = "gosset")
#' data("breadwheat", package = "gosset")
#' 
#' # compute all temperature indices for the first 30 days in the plots
#' temperature(breadwheat_modis,
#'             day.one = breadwheat["planting_date"],
#'             span = 30)
#' 
#' 
#' ########################################
#' 
#' # Temperature indices from NASA POWER
#' 
#' library("nasapower")
#' library("raster")
#' 
#' temperature(breadwheat[c("lon","lat")], 
#'             day.one = breadwheat["planting_date"], 
#'             span = 40)
#' 
#' @import nasapower
#' @import raster
#' @export
temperature <- function(object, day.one = NULL, span = NULL,
                        index = NULL, ...)
  {

  if (is.null(index)) {
    index <- c("maxDT","minDT","maxNT","minNT","SU","TR","DTR")
  }

  # get timespan for the day temperature
  if (dim(object)[2] == 2) {
    cat("fetching NASA POWER, this may take a little longer. \n")
    day <- .get_timespan(object, day.one, span, pars = "T2M_MAX", ...)
  } else {
    day <- .get_timespan(object[, , 1], day.one, span, ...)
  }
  
  # get timespan for the night temperature
  if (dim(object)[2] == 2) {
    night <- .get_timespan(object, day.one, span, pars = "T2M_MIN", ...)
  } else {
    night <- .get_timespan(object[, , 2], day.one, span, ...)
  }
  
  n <- nrow(day)

  ind <- tibble::as_tibble(matrix(nrow = n, ncol = length(index), dimnames = list(1:n, index)))

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


#' Rainfall indices
#'
#' Compute rainfall indices over a timespan
#'
#' @param object a numeric vector of geographic coordinates (lonlat) or
#' a matrix containing the precipitation data.
#' When lonlat is used, the function makes a call to
#' \code{nasapower::get_power} to fetch and concatenate environmental data from NASA POWER
#' (\url{https://power.larc.nasa.gov/})
#' @inheritParams temperature
#' @return A dataframe with selected indices. Options are:
#' \item{MLDS}{maximum length of consecutive dry days (r <  1 mm)}
#' \item{MLWS}{maximum length of consecutive wet days (r >= 1 mm)}
#' \item{R10mm}{number of heavy precipitation days (10 >= r < 20 mm)}
#' \item{R20mm}{number of very heavy precipitation days (r >= 20) }
#' \item{SDII}{simple daily intensity index (mean of wet days / total rainfall)}
#' \item{Rx1day}{maximum 1-day rainfall (mm)}
#' \item{Rx5day}{maximum 5-day rainfall (mm) }
#' \item{Rtotal}{total rainfall (mm) in wet days (R >= 1)}
#' @examples
#' 
#' # Compute indices using precipitation data from CHIRPS
#' data("breadwheat_chirps", package = "gosset")
#' data("breadwheat", package = "gosset")
#' 
#' # The rainfall indices for the first 50 days after sowing
#' rainfall(breadwheat_chirps, 
#'          day.one = breadwheat$planting_date, 
#'          span = 50)
#' 
#' ########################################
#' 
#' # Add the first 7 days before sowing (residual precipitation)
#' rainfall(breadwheat_chirps, 
#'          day.one = breadwheat$planting_date, 
#'          span = 50,
#'          days.before = 7)
#' 
#' ########################################
#' 
#' # Compute indices using NASA POWER
#' 
#' library("nasapower")
#' library("raster")
#' 
#' rainfall(breadwheat[c("lon","lat")], 
#'          day.one = breadwheat$planting_date, 
#'          span = 50)
#'          
#' @export
rainfall <- function(object, day.one = NULL, span = NULL,
                           index = NULL, ...)
  {

  if (is.null(index)) {
    index <- c("MLDS","MLWS","R10mm","R20mm","SDII","Rx1day","Rx5day","Rtotal")
  }

  #get timespan
  if (dim(object)[2] == 2) {
    cat("fetching NASA POWER, this may take a little longer. \n")
    
    r <- .get_timespan(object, day.one, span, pars = "PRECTOT", ...)
  } else {
    r <- .get_timespan(object, day.one, span, ...)
  }

  n <- nrow(r)

  ind <- tibble::as_tibble(matrix(nrow = n, ncol = length(index), dimnames = list(1:n, index)))

  # maximum length of consecutive dry days (< 1 mm)
  if ("MLDS" %in% index) {
    ind["MLDS"]   <- .dryspell(r)
  }
  # maximum length of consecutive wet days
  if ("MLWS" %in% index) {
    ind["MLWS"]   <- .wetspell(r)
  }
  # days with rainfall between 10-15 mm
  if ("R10mm" %in% index) {
    ind["R10mm"]  <- apply(r, 1, function(x) {
        sum(x >= 10 & x < 20, na.rm = TRUE)
      })
  }
  # days with rainfall > 20 mm
  if ("R20mm" %in% index) {
    ind["R20mm"]  <- apply(r, 1, function(x) {
      sum(x >= 20, na.rm = TRUE)
    })
  }
  # simple rainfall intensity index
  if ("SDII" %in% index) {
    ind["SDII"] <- apply(r, 1, function(x) {
        sum(x, na.rm = TRUE) / sum(x >= 1, na.rm = TRUE)
      })
  }
  # maximum 1-day rainfall
  if ("Rx1day" %in% index) {
    ind["Rx1day"] <- apply(r, 1, function(x) {
      max(x, na.rm = TRUE)
    })
  }
  # maximum consecutive 5-day precipitation
  if ("Rx5day" %in% index) {
    ind["Rx5day"] <- .rx5day(r)
  }
  # total rainfall (mm) in wet days (r >= 1)
  if ("Rtotal" %in% index) {
    ind["Rtotal"] <- apply(r, 1, function(x) {
        sum(x[as.vector(x >= 1)], na.rm = TRUE)
      })
  }

  ind[is.na(ind)] <- 0

  return(ind)
}


#' Evapotranspiration
#' 
#' Compute evapotranspiration using the Blaney-Criddle method
#' 
#' @inheritParams temperature
#' @param lat the latitude (in Decimal degrees)
#' @param Kc the crop factor for water requirement
#' @param p optional, a numeric value (from 0 to 1) used if lat is not given,
#' representing the mean daily percentage of annual daytime hours 
#' for different latitudes
#' @return The evapotranspiration in mm/day
#' @examples
#'  
#' # Use temperature data from MODIS MYD11A1
#' data("breadwheat_modis", package = "gosset")
#' data("breadwheat", package = "gosset")
#' 
#' # the evapotranspiration in the first 100 days after planting
#' ETo(breadwheat_modis, 
#'     day.one = breadwheat$planting_date,
#'     span = 100,
#'     lat = breadwheat$lat)
#' 
#' @export
ETo <- function(object, day.one = NULL, span = NULL, 
                lat = NULL, Kc = 1, p = NULL){

  # get p if lat is provided
  if (!is.null(lat)) {
    l <- round5(lat, 5)
    m <- as.integer(format(day.one, "%m"))
    p <- daylight[cbind(match(l , daylight[, 1]), match(m , names(daylight)))]
  } 
  
  if (is.null(p)) {
    p <- 0.27
  }
  
  # get timespan for the day temperature
  if (dim(object)[2] == 2) {
    cat("fetching NASA POWER, this may take a little longer. \n")
    day <- .get_timespan(object, day.one, span, pars = "T2M_MAX")
  } else {
    day <- .get_timespan(object[, , 1], day.one, span)
  }

  # get timespan for the night temperature
  if (dim(object)[2] == 2) {
    night <- .get_timespan(object, day.one, span, pars = "T2M_MIN")
  } else {
    night <- .get_timespan(object[, , 2], day.one, span)
  }

  # calculate Tmean
  Tmean <- (rowMeans(day, na.rm = TRUE) +  rowMeans(night, na.rm = TRUE)) / 2

  # evapotranspiration
  eto <- p * (0.46 * Tmean + 8) * Kc

  return(tibble::tibble(ETo = eto))

}


# compute Rx5day rainfall index
.rx5day <- function(object)
{
  # this look for the maximum sum of rain in
  # consecutive 5 days
  l <- ncol(object)
  Y <- apply(object, 1, function(Y){
    r5day <- NULL
    for(i in 1:(l-4)){
      r5day <- cbind(r5day, sum(Y[i:(i+4)], na.rm = TRUE))}
    return(max(r5day, na.rm = TRUE))
  })
  return(Y)
}

# compute the maximum length of consecutive dry days
.dryspell <- function(object)
{
  # the function rle is applied
  # which looks for the sequencies of numbers
  # in this case, zeros (0)
  # take the maximum sequency
  f <- apply(object, 1, function(Y){
    # first all values < 1 are converted to zero (0)
    Y <- ifelse(Y < 1, 0, Y)
    # get the lengths of each sequency of zeros (0)
    keep <- rle(Y)$values
    keep <- keep == 0
    Y <- rle(Y)$lengths[keep]
    # if there is no value (empty()) then set as zero
    if (length(Y) == 0) {
      Y <- 0
    }
    # if there is values, take the maximum sequecy
    if (length(Y) != 0) {
      Y <- max(Y, na.rm = TRUE)
    }
    return(Y)
  }  )
  return(f)
}

# compute the maximum length of consecutive wet days
.wetspell <- function(object)
{
  # the function rle is applied
  # which looks for the sequencies of zeros
  # take the maximum sequency
  f <- apply(object, 1, function(Y){
    # first all values >= 1 are converted to zero (0)
    # no precipitation (r < 1) is converted to two (2)
    Y <- ifelse(Y >= 1, 0, 2)
    # get the lengths of each sequency of zeros (0)
    keep <- rle(Y)$values
    keep <- keep == 0
    Y <- rle(Y)$lengths[keep]
    # if there is no value (empty()) then set as zero
    if (length(Y) == 0) {
      Y <- 0
    }
    # if there is values, take the maximum sequecy
    if (length(Y) != 0) {
      Y <- max(Y, na.rm = TRUE)
    }
    return(Y)
  }  )
  return(f)
}

# Time series environmental data
# 
# Concatenate time series environmental data
# 
# @param object a numeric vector of geographic coordinates (lonlat) or
# a matrix with environmental data from other sources.
# When lonlat is used, the function makes a call to
# \code{nasapower::get_power} to fetch and combine enviromental data from NASA POWER
# (\url{https://power.larc.nasa.gov/})
# @param day.one a vector of class 'Date' for the starting date to capture the environmental data
# @param span an integer or a vector with integers for the duration of the timespan to be captured
# @param days.before optional, an integer for the number of days before
# start.date to be included in the timespan
# @param ... additional arguments passed to \code{\link[nasapower]{get_power}}
# @return a data frame of environmental data for the chosen period
# @examples
# 
# # Concatenate environmental data from from CHIRPS
# data("breadwheat_chirps", package = "gosset")
# data("breadwheat", package = "gosset")
# 
# # Get the precipitation that occured in the first 30 days after the planting date
# get_timespan(breadwheat_chirps,
#              day.one = breadwheat$planting_date,
#              span = 30)
# 
# ######################################
# 
# # Get data from NASA POWER
# 
# # select daily precipitation using the argument 'pars'
# get_timespan(breadwheat[c("lon","lat")],
#              day.one = breadwheat$planting_date,
#              span = 30,
#              pars = "PRECTOT")
.get_timespan <- function(object, day.one = NULL,
                          span = NULL, days.before = NULL, ...)
{
  
  if (is.null(day.one)) {
    stop("argument 'day.one' is missing with no default \n")
  }
  
  if (is.null(span)) {
    span <- 150
  }
  
  if (is.null(days.before)) {
    days.before <- 0
  }
  
  if (is.matrix(object)) {
    object <- as.data.frame(object)
  }
  
  if (any(c("tbl_df", "tbl") %in% class(day.one))) {
    day.one <- day.one[[1]]
  }
  
  # the timespan
  span <- as.vector(t(span))
  # the begin date
  b <- day.one - days.before
  # the end date
  e <- day.one + span
  # the refreshed timespan
  span <- as.integer(e - b)
  # the maximum timespan
  maxspan <- max(span)
  
  # look if nasapower is required 
  nasa_power <- dim(object)[[2]] == 2
  
  # then get data from NASA POWER using nasapower::get_power
  if (nasa_power) {
    
    # define geographic boundaries for lonlat
    lims <- with(object, c(min(object[,1]), min(object[,2]),
                           max(object[,1]), max(object[,2])))
    # the first and last date to fetch
    dates <- c(min(b), max(e))
    # the projection
    myproj <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    
    # get NASA POWER
    info <- nasapower::get_power(community = "AG",
                                 lonlat = lims,
                                 dates = dates,
                                 temporal_average = "DAILY", 
                                 ...)
    
    # split by YYYYMMDD to create a list of data frames
    info <- split(info, info$YYYYMMDD)
    
    # keep only coordinates and the variable fetched
    info <- lapply(info, function(x) {
      x[(!names(x) %in% c("YEAR", "MM", "DD", "DOY", "YYYYMMDD"))]
    })
    
    # create a list of raster bricks from each YYYYMMDD data frame
    r <- lapply(info, function(x) {
      raster::rasterFromXYZ(x, res = c(0.5, 0.5), crs = myproj)
    })
    
    # stack the raster
    r <- raster::stack(unlist(r))
    
    #extract the information using lonlat points provided in 'object'
    object <- raster::extract(r, object)
    
    object <- as.data.frame(object)
    
    names(object) <- as.character(names(info))
    
  }
  
  # check if provided start.date exists within the object
  days <- names(object)
  out <- !as.character(b) %in% days & !as.character(e) %in% days
  
  if (any(out)) {
    suppress <- dimnames(object)[[1]][out]
    warning("Ignoring rows c(",
            toString(suppress),
            "): subscript out of bounds \n")
    rnames <- as.integer(dimnames(object)[[1]][!out])
  } else{
    rnames <- as.integer(dimnames(object)[[1]])
  }
  
  # refresh objects
  object <- object[!out, ]
  b <- b[!out]
  span <- span[!out]
  maxspan <- max(span)
  n <- dim(object)[1]
  rownames(object) <- 1:n
  
  # find the index for specified dates within the start.dates provided
  date <- match(as.character(b), days)
  
  Y <- NULL
  
  for (i in 0:maxspan) {
    Y <- cbind(Y, object[cbind(1:n, date + i)])
  }
  
  # if ts is variable then add NA's
  Y <- t(apply(cbind(span, Y), 1, function(x) {
    x1 <- x[1]
    
    x <- x[2:length(x)]
    
    x[(x1 + 1):length(x)] <- NA
    
    return(x)
    
  }))
  
  # make a tibble
  if (n != 1) {
    Y <- tibble::as_tibble(Y[, 1:maxspan])
  } else {
    Y <- tibble::as_tibble(t(Y[, 1:maxspan]))
  }
  
  names(Y) <- paste0("day", 1:ncol(Y))
  
  return(Y)
  
}

# Growing Degree Days
#
# Compute number of days required to reach growing degree days.
# GDD are calculated by taking the integral of warmth above a base temperature.
#
# @param object a numeric vector of geographic coordinates (lonlat) or
# an array with two dimensions containing the temperature data;
# 1st dimension contains the day temperature and 2nd dimension the night temperature.
# When lonlat is used, the function makes a call to
# nasapower::get_power to fetch and combine enviromental information from NASA POWER API
# \url{https://power.larc.nasa.gov/}.
# @param start.date a vector of class 'Date' for the starting date to capture
# the environmental information.
# @param degree.days an integer for the degree days required by the crop (look at the physiology of the focal crop)
# @param base an integer for the base temperature. Set 10 as default.
# @param ts an integer or a vector for the duration of
# the timespan to be captured. Set as 150 by default.
# @return The number of days required to reach the growing degree days.
# @examples
# # Example 1
# # Use temperature data extracted from MODIS MYD11A1
# # (http://dx.doi.org/10.5067/MODIS/MYD11A2.006) for the wheat data
# # each row in wheat corresponds to the same rows in temperature,
# # first layer is the day temperature
# # and second layer is the night temperature
# data("temperature")
# data("wheat")
#
# # Calculate the days required for the plants in these plots to reach the
# # maturity. The crop requires ~1800 degree days for it.
# GDD(temperature, start.date = wheat["planting_date"],
#     degree.days = 1800, base = 5)
#
#
# # @export
# 
# GDD <- function(object, start.date = NULL, degree.days = NULL,
#                  base = NULL, ts = NULL)
# {
#   
#   # validate parameters
#   if (is.null(start.date)) {
#     stop("a Date vector for start.date is required \n")
#   }
#   if (is.null(degree.days)) {
#     stop("an integer for degree.days is required \n")
#   }
#   if (is.null(base)) {
#     base <- 10
#   }
#   if (is.null(ts)) {
#     ts <- 150
#   }
#   
#   # get timespan for the day temperature
#   if (dim(object)[2] == 2) {
#     day <- get_timespan(object, start.date, ts, pars = "T2M_MAX")
#   } else {
#     day <- get_timespan(object[, , 1], start.date, ts)
#   }
#   
#   # get timespan for the night temperature
#   if (dim(object)[2] == 2) {
#     night <- get_timespan(object, start.date, ts, pars = "T2M_MIN")
#   } else {
#     night <- get_timespan(object[,,2], start.date, ts)
#   }
#   
#   # get the difference between day and night temperature
#   Y <- (((day + night) / 2) - base)
#   
#   # sum temperature values until reach the defined degree days
#   Y <- apply(Y, 1, function(x){
#     for(d in 1:length(x)){
#       i=d
#       if(sum(x[1:d]) > degree.days) break}
#     return(i)
#   })
#   
#   return(tibble::tibble(GDD=Y))
# }

