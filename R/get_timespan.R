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
#' @import nasapower
#' @importFrom raster rasterFromXYZ extract stack
.get_timespan <- function(object, day.one = NULL,
                          span = NULL, days.before = NULL, ...)
{
  
  if (is.null(day.one)) {
    stop("argument 'day.one' is missing with no default \n")
  }
  
  if (is.null(span)) {
    stop("argument 'span' is missing with no default \n ")
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
  
  # the maximum end date
  maxend <- max(b) + max(span)
  
  # look if nasapower is required 
  nasa_power <- dim(object)[[2]] == 2
  
  # then get data from NASA POWER using nasapower::get_power
  if (nasa_power) {
    
    # define geographic boundaries for lonlat
    lims <- with(object, c(floor(min(object[,1])), 
                           floor(min(object[,2])),
                           ceiling(max(object[,1])), 
                           ceiling(max(object[,2]))))
    
    # the first and last date to fetch
    dates <- c(min(b), maxend)
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
  
  # # check if provided start.date exists within the object
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




