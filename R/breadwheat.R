#' Preferred bread wheat varieties
#'
#' Data from crowdsourcing citizen-science trials of bread wheat 
#' (\emph{Triticum aestivum}) varieties in Vaishali, India over the 
#' 2014's Rabi season. Farmers were asked to test three varieties 
#' of bread wheat randomly assigned from a total of 16 varieties 
#' and report which one had the best and worst overall performance. 
#' 
#' @format A data frame with 493 records and 8 variables:
#' \describe{
#'     \item{\code{variety_a}}{The name of variety A in the comparison.}
#'     \item{\code{variety_b}}{The name of variety B in the comparison.}
#'     \item{\code{variety_c}}{The name of variety C in the comparison.}
#'     \item{\code{planting_date}}{The date which the experiment started.}
#'     \item{\code{lon}}{The longitude in which the experiment was established.}
#'     \item{\code{lat}}{The latitude in which the experiment was established.}
#'     \item{\code{best}}{The variety the farmer ranked as best ("A",
#'     "B" or "C").}
#'     \item{\code{worst}}{The variety the farmer ranked as worst ("A",
#'     "B" or "C").}
#' }
#' @source The data were provided by Bioversity International, a CGIAR Research
#' Centre, through the ClimMob platform \url{https://climmob.net}.
"breadwheat"
