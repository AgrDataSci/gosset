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
#' @references 
#' Aguilar E., et al. (2005). Journal of Geophysical Research, 110(D23), D23107. https://doi.org/10.1029/2005JD006119.
#' 
#' Kehel Z., et al. (2016). Identifying Climate Patterns during the Crop-Growing Cycle from 30 Years of CIMMYT Elite Spring Wheat International Yield Trials. In: Applied Mathematics and Omics to Assess Crop Genetic Resources for Climate Change Adaptive Traits (eds Bari A., Damania A. B., Mackay M., Dayanandan S.), pp. 151–174. CRC Press.
#' 
#' Sparks A. H. (2018). Journal of Open Source Software, 3(30), 1035. https://doi.org/10.21105/joss.01035.
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
#' # calculate rainfall for the first 50 days after planting
#' rainfall(lonlat,
#'          day.one = pdates,
#'          span = 50)
#' 
#' 
#' # include the first 15 days before planting (residual precipitation)
#' rainfall(lonlat,
#'          day.one = pdates,
#'          span = 50,
#'          days.before = 15)
#' 
#' }       
#'          
#' @export
rainfall <- function(object, day.one = NULL, span = 150,
                     index = c("MLDS","MLWS","R10mm","R20mm","SDII","Rx1day","Rx5day","Rtotal"), 
                     ...)
{
  
  # get timespan
  if (dim(object)[2] == 2) {
    r <- .get_timespan(object, day.one, span, pars = "PRECTOT", ...)
  } else {
    r <- .get_timespan(object, day.one, span, ...)
  }
  
  n <- nrow(r)
  
  ind <- tibble::as_tibble(matrix(nrow = n, 
                                  ncol = length(index), 
                                  dimnames = list(1:n, index)))
  
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