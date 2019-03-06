#' MODIS data from preferred bread wheat varieties trial locations
#'
#' Data from MODIS (MYD11A2) containing the time series temperature data of the bread wheat 
#' (\emph{Triticum aestivum}) citizen-science trails during the 2014's Rabi season. The data 
#' covers a timespan of 198 days from the date that the first experiment started. 
#' 
#' @format A array with 493 records, 198 colunms and 2 layers:
#' \describe{
#'     \item{\code{rows}}{The temperature data for each observation in the breadwheat data.}
#'     \item{\code{colunms}}{Days, from 2014-10-31 to 2015-05-16.}
#'     \item{\code{layer 1}}{The day temperature in Celsius degrees.}
#'     \item{\code{layer 2}}{The night temperature in Celsius degrees.}
#' }
#' @seealso \code{\link{breadwheat}}
#' @source The data were obtained from MODIS (MYD11A2) \url{http://dx.doi.org/10.5067/MODIS/MYD11A2.006}.
"breadwheat_modis"
