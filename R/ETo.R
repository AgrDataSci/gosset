#' Evapotranspiration
#' 
#' Compute evapotranspiration using the Blaney-Criddle method
#' 
#' @inheritParams temperature
#' @param lat a vector for the latitude (in Decimal degrees)
#' @param Kc a numeric value for the crop factor for water requirement
#' @param p optional, a numeric value (from 0 to 1) used if lat is not given,
#' representing the mean daily percentage of annual daytime hours 
#' for different latitudes
#' @return The evapotranspiration in mm/day
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
#' # the evapotranspiration in the first 50 days after planting
#' ETo(lonlat,
#'     day.one = pdates,
#'     span = 50,
#'     lat = lonlat[["lat"]])
#' }
#' 
#' @export
ETo <- function(object, day.one = NULL, span = NULL, 
                lat = NULL, Kc = 1, p = NULL){
  
  # remove vector from a tibble object
  if (.is_tibble(day.one)) {
    day.one <- day.one[[1]]
  }
  
  # remove vector from a tibble object
  if (.is_tibble(lat)) {
    lat <- lat[[1]]
  }
  
  # get p if lat is provided
  if (!is.null(lat)) {
    l <- .round5(lat, 5)
    m <- as.integer(format(day.one, "%m"))
    p <- daylight[cbind(match(l , daylight[, 1]), match(m , names(daylight)))]
  } 
  
  if (is.null(p)) {
    p <- 0.27
  }
  
  # get timespan for the day temperature
  if (dim(object)[2] == 2) {
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


