#' Rainfall indices
#'
#' Compute rainfall indices over a timespan
#'
#' @param object a numeric vector of geographic coordinates (lonlat) or
#' a matrix containing the precipitation data.
#' When lonlat is used, the function makes a call to
#' \code{nasapower::get_power} to fetch and concatenate environmental 
#' data from NASA POWER (\url{https://power.larc.nasa.gov/})
#' @inheritParams temperature
#' @return A dataframe with rainfall indices:
#' \item{MLDS}{maximum length of consecutive dry day, rain < 1 mm (days)}
#' \item{MLWS}{maximum length of consecutive wet days, rain >= 1 mm (days)}
#' \item{R10mm}{number of heavy precipitation days 10 >= rain < 20 mm (days)}
#' \item{R20mm}{number of very heavy precipitation days rain >= 20 (days)}
#' \item{Rx1day}{maximum 1-day precipitation (mm)}
#' \item{Rx5day}{maximum 5-day precipitation (mm)}
#' \item{R95p}{total precipitation when rain > 95th percentile (mm)}
#' \item{R99p}{total precipitation when rain > 99th percentile (mm)}
#' \item{Rtotal}{total precipitation (mm) in wet days, rain >= 1 (mm)}
#' \item{SDII}{simple daily intensity index, total precipitation divided by the
#'  number of wet days (mm/days)}
#'  
#'  When \var{timeseries} = \code{TRUE}, an id is created, 
#'  which is the index for the rownames of the provided \var{object}.
#'  
#' @family climatology functions
#' @references 
#' Aguilar E., et al. (2005). Journal of Geophysical Research, 
#' 110(D23), D23107. \cr\url{https://doi.org/10.1029/2005JD006119}
#' 
#' Kehel Z., et al. (2016). Identifying Climate Patterns during the 
#' Crop-Growing Cycle from 30 Years of CIMMYT Elite Spring Wheat 
#' International Yield Trials. In: Applied Mathematics and Omics to 
#' Assess Crop Genetic Resources for Climate Change Adaptive Traits 
#' (eds Bari A., Damania A. B., Mackay M., Dayanandan S.), 
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
#' # random dates within 2018-05-15 and 2018-05-20
#' set.seet(321)
#' pdates <- as.integer(runif(3, 17666, 17670))
#' pdates <- as.Date(pdates, origin = "1970-01-01")
#' 
#' # calculate rainfall for the first 50 days after day.one
#' rainfall(lonlat,
#'          day.one = pdates,
#'          span = 50)
#' 
#' 
#' # include the first 15 days before day.one (residual precipitation)
#' rainfall(lonlat,
#'          day.one = pdates,
#'          span = 50,
#'          days.before = 15)
#'          
#' # rainfall indices over a time series          
#' rainfall(lonlat,
#'          day.one = pdates,
#'          span = 50,
#'          timeseries = TRUE,
#'          intervals = 7)
#' }       
#'          
#' @export
rainfall <- function(object, day.one = NULL, 
                     span = 150, timeseries = FALSE,
                     intervals = 5,
                     ...)
{
  
  index <- c("MLDS","MLWS","R10mm","R20mm","Rx1day",
             "Rx5day","R95p","R99p","Rtotal","SDII")
  
  # get timespan
  if (dim(object)[2] == 2) {
    r <- .get_timeseries(object, day.one, span, pars = "PRECTOT", ...)
  } else {
    r <- .get_timeseries(object, day.one, span, ...)
  }
  
  n <- nrow(r)
  
  if (timeseries) {
    
    # it might happen that when bins are not well distributed across dates
    # in that case the last values are dropped
    # for example, divide the periods of 7 days in a time series of 53 days
    # in that case, the last four observations are dropped to fit in a vector of
    # length == 49 (the maximum integer from dividing days/intervals)
    # organise bins, ids and dates
    bins <- floor(ncol(r)/intervals)
    
    bins <- rep(1:bins, each = intervals, length.out = NA)
    
    # ids are the row names in r
    ids <- rownames(r)
    
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
    rr <- t(r)
    
    # keep data within the lenght of bins
    rr <- as.data.frame(rr[1:length(bins), ])
    
    # split by ids
    rr <- split(rr, bins)
    
    # calculate indices
    ind <- lapply(rr, function(x) {
      
      x <- apply(x, 2, function(y) {
        
        c(.dryspell(y),
          .wetspell(y),
          .r_ten_mm(y),
          .r_twenty_mm(y),
          .r_one_day(y),
          .r_five_day(y),
          .very_wet_days(y),
          .extrem_wet_days(y),
          .r_total(y),
          .sdii(y))
        
      })
      
      x <- data.frame(id    = rep(ids, each = length(index)),
                      index = rep(index, n), 
                      value = as.vector(x))  
      
      })
    
    ind <- do.call("rbind", ind)
    
    ind$id <- as.integer(ind$id)
    
    ind$index <- as.character(ind$index)
    
    ind <- ind[order(ind$id), ]
    
    ind$dates <- dates
    
    ind <- ind[, c("id", "dates", "index", "value")]
    
    
  } 
  
  # if no time series required then
  if (!timeseries) {
    
    # split r by rows
    r <- split(r, 1:nrow(r))
    
    ind <- lapply(r, function(x) {
      
      x <- as.vector(as.matrix(x))
      
      x <- data.frame(.dryspell(x),
                      .wetspell(x),
                      .r_ten_mm(x),
                      .r_twenty_mm(x),
                      .r_one_day(x),
                      .r_five_day(x),
                      .very_wet_days(x),
                      .extrem_wet_days(x),
                      .r_total(x),
                      .sdii(x))
      
    })
    
    
    ind <- do.call("rbind", ind)
    
    names(ind) <- index
    
  }
  
  ind <- tibble::as_tibble(ind)
  
  return(ind)
}


#' Maximum length of consecutive dry days
#' @param object numeric vector
#' @return the MLDS index, which is the maximum length of consecutive dry days
#' precipitation < 1 mm
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.dryspell(r)
#' @noRd
.dryspell <- function(object)
{
  # the function rle is applied
  # which looks for the sequencies of numbers
  # in this case, zeros (0)
  # take the maximum sequency
  # first all values < 1 are converted to zero (0)
  ds <- ifelse(object < 1, 0, object)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(ds)$values
  
  keep <- keep == 0
  
  ds <- rle(ds)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  # which means there is no dry spell
  if (length(ds) == 0) {
    ds <- 0
  }
  
  # if there is values, take the maximum sequency
  if (length(ds) != 0) {
    ds <- max(ds, na.rm = TRUE)
  }
  
  return(ds)
  
}

#' Maximum length of consecutive wet days
#' @param object numeric vector
#' @return the MLWS index, which is the maximum length of consecutive wet days
#' precipitation > 1 mm
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:11)] <- 0.1
#' chirps:::.wetspell(r)
#' @noRd
.wetspell <- function(object)
{
  # the function rle is applied
  # which looks for the sequencies of zeros
  # take the maximum sequency
  # first all values >= 1 are converted to zero (0)
  # no precipitation (r < 1) is converted to two (2)
  ws <- ifelse(object >= 1, 0, 2)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(ws)$values
  
  keep <- keep == 0
  
  ws <- rle(ws)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  if (length(ws) == 0) {
    ws <- 0
  }
  # if there is values, take the maximum sequecy
  if (length(ws) != 0) {
    ws <- max(ws, na.rm = TRUE)
  }
  
  return(ws)
}

#' Heavy precipitation days (10 >= r < 20 mm)
#' @param object numeric vector
#' @return the R10mm index, which is number of heavy precipitation days (10 >= r
#'  < 20 mm)
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 12)
#' r[c(1,4,9:11)] <- 0.1
#' chirps:::.r_ten_mm(r)
#' @noRd
.r_ten_mm <- function(object) {
  
  rt <- sum(object >= 10 & object < 20, na.rm = TRUE)
  
  return(rt)
  
}

#' Very heavy precipitation days (r >= 20)
#' @param object numeric vector
#' @return the R20mm index, which is number of very heavy precipitation days (r
#'  >= 20)
#' @examples
#' set.seed(12)
#' r <- runif(20, 10, 23)
#' r[c(1,4,9:11)] <- 0.1
#' chirps:::.r_twenty_mm(r)
#' @noRd
.r_twenty_mm <- function(object) {
  
  rtw <- sum(object >= 20, na.rm = TRUE)
  
  return(rtw)
  
}

#' Simple rainfall intensity index
#' @param object numeric vector
#' @return the SDII index, which is the simple daily intensity 
#' index total precipitation divided by the number of wet days (r >= 1.0mm)
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#'
#' r[c(1,4,9:11)] <- 0.1
#'
#' .sdii(r)
#'
#' chirps:::.sdii(rep(0.1, 9))
#' @noRd
.sdii <- function(object) {
  
  # total precipitation
  tp <- sum(object, na.rm = TRUE)
  
  # number of wet days
  wd <- length(object[object >= 1])
  
  #if both zero, then return 0
  if (wd == 0) {
    si <- 0L
  }else{
    si <- tp / wd
  }
  
  return(si)
  
}

#' Compute Rx5day rainfall index
#' @param object numeric vector
#' @return the Rx5day index, which is the maximun sum 
#' of rain in consecutive 5 days
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.r_five_day(r)
#' @noRd
.r_five_day <- function(object)
{
  # this look for the maximum sum of rain in
  # consecutive 5 days
  l <- length(object)
  
  r5day <- NULL
  
  for (i in 1:(l-4)){
    
    r5day <- cbind(r5day, sum(object[i:(i + 4)], na.rm = TRUE))
    
  }
  
  r5day <- max(r5day, na.rm = TRUE)
  
  return(r5day)
  
}

#' Maximum 1-day rainfall
#' @param object numeric vector
#' @return the Rx1day index, which is the 1-day rainfall
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.r_one_day(r)
#' @noRd
.r_one_day <- function(object) {
  
  ro <- max(object, na.rm = TRUE)
  
  return(ro)
  
}

#' Total rainfall (mm) in wet days (r >= 1)
#' @param object numeric vector
#' @return the Rtotal index, which is sum of rainfall (mm) in wet days (r >= 1)
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.r_total(r)
#' @noRd
.r_total <- function(object) {
  
  rt <- object[object >= 1]
  
  rt <- sum(object, na.rm = TRUE)
  
  return(rt)
  
}


#' Very wet days
#' @param object numeric vector
#' @return the R95p index, annual total PRCP when rain > 95th percentile
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.very_wet_days(r)
#' @noRd
.very_wet_days <- function(object) {
  
  q <- stats::quantile(object, probs = seq(0, 1, 0.05), na.rm = TRUE)
  q <- q["95%"]
  
  vwd <- object[object > q]
  
  vwd <- sum(vwd, na.rm = TRUE)
  
  return(vwd)
  
}

#' Very wet days
#' @param object numeric vector
#' @return the R95p index, annual total PRCP when rain > 95th percentile
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.extrem_wet_days(r)
#' @noRd
.extrem_wet_days <- function(object) {
  
  q <- stats::quantile(object, probs = seq(0, 1, 0.01), na.rm = TRUE)
  q <- q["99%"]
  
  vwd <- object[object > q]
  
  vwd <- sum(vwd, na.rm = TRUE)
  
  return(vwd)
  
}
