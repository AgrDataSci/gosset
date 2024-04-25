#' Preferred bread wheat varieties
#'
#' Data from decentralized on-farm trials of bread wheat 
#' (\emph{Triticum aestivum} L.) varieties in Vaishali, India over the 
#' 2014's Rabi season. Farmers were asked to test three varieties 
#' of bread wheat randomly assigned as incomplete blocks of 
#' three varieties (out of 16 varieties) and assess which one had
#' the best and worst performance considering 
#' four traits, germination, grain quality, yield and overall performance. 
#' 
#' @keywords dataset
#' @format A data frame with 493 records and 19 variables:
#' \describe{
#'     \item{\code{variety_a}}{The name of variety A in the comparison.}
#'     \item{\code{variety_b}}{The name of variety B in the comparison.}
#'     \item{\code{variety_c}}{The name of variety C in the comparison.}
#'     \item{\code{district}}{The administrative region where the experiment was stablished.}
#'     \item{\code{village}}{The village within the administrative region where the 
#'      experiment was stablished.}
#'     \item{\code{participant_name}}{The participant name (ommited for protection and privacy).}
#'     \item{\code{age}}{The participant age.}
#'     \item{\code{gender}}{The participant gender M = Male; F = Female.}
#'     \item{\code{planting_date}}{The date which the experiment started.}
#'     \item{\code{lon}}{The longitude in which the experiment was stablished.}
#'     \item{\code{lat}}{The latitude in which the experiment was stablished.}
#'     \item{\code{germination_best}}{The variety ranked as best for germination ("A",
#'     "B" or "C").}
#'     \item{\code{germination_worst}}{The variety ranked as worst for germination ("A",
#'     "B" or "C").}
#'     \item{\code{grainquality_best}}{The variety ranked as best for grain quality ("A",
#'     "B" or "C").}
#'     \item{\code{grainquality_worst}}{The variety ranked as worst for grain quality ("A",
#'     "B" or "C").}
#'     \item{\code{yield_best}}{The variety ranked as best for yield ("A",
#'     "B" or "C").}
#'     \item{\code{yield_worst}}{The variety ranked as worst for yield ("A",
#'     "B" or "C").}
#'     \item{\code{overall_best}}{The variety ranked as best for overall perfomance ("A",
#'     "B" or "C").}
#'     \item{\code{overall_worst}}{The variety ranked as worst for overall perfomance ("A",
#'     "B" or "C").}
#' }
#' @source
#' van Etten, J., et. al. (2016). 
#' Experimental Agriculture, 55, 275-296.
#' \doi{https://doi.org/10.1017/S0014479716000739}
#' 
#' van Etten, J., et. al. (2019). 
#' PNAS 116(10) 4194-4199
#' \doi{https://doi.org/10.1073/pnas.1813720116}
"breadwheat"
