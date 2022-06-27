#' Common bean on-farm trial in Nicaragua
#'
#' Data from decentralized on-farm trials of common bean 
#' (\emph{Phaseolus vulgaris} L.) varieties in Nicaragua over
#' five seasons between 2015 and 2016. Following the tricot approach, 
#' farmers were asked to test three varieties of common bean randomly 
#' assigned as incomplete blocks of three varieties (out of 10 varieties) 
#' and assess which of those three had the best and worst performance in 
#' nine traits (Vigor, Architecture, Resistance to Pests, Resistance to 
#' Diseases, Tolerance to Drought, Yield, Marketability, Taste, and Overall 
#' Appreciation). 
#'  
#' @keywords dataset
#' @format A list with two data frames, 
#'  \code{nicabean[["trial"]]} contains the trial data:
#'  \describe{
#'  \item{\code{id}}{the plot id}
#'  \item{\code{item}}{the variety name}
#'  \item{\code{trait}}{the trait for the given variety and plot id}
#'  \item{\code{rank}}{the rank for the given variety and trait, with 1 being higher and 3 the lowest}
#'  }
#'  \code{nicabean[["bean_covar"]]} contains the covariates associated with the data:
#'  \describe{
#'  \item{\code{id}}{the plot id}
#'  \item{\code{adm0}}{the country name where trials were set}
#'  \item{\code{longitude}}{the longitude of the trial plot}
#'  \item{\code{latitude}}{the latitude of the trial plot}
#'  \item{\code{trial}}{the trial name as registered on ClimMob}
#'  \item{\code{variety_a}}{the variety assigned as label A in the incomplete block}
#'  \item{\code{variety_b}}{the variety assigned as label B in the incomplete block}
#'  \item{\code{variety_c}}{the variety assigned as label C in the incomplete block}
#'  \item{\code{planting_date}}{the planting date}
#'  \item{\code{gender}}{the farmer gender}
#'  \item{\code{age}}{the farmer age}
#'  }
#' @source
#' van Etten, J., et. al. (2016). 
#' Experimental Agriculture, 55, 275-296.
#' \doi{https://doi.org/10.1017/S0014479716000739}
#' 
#' van Etten, J., et. al. (2019). 
#' PNAS 116(10) 4194-4199
#' \doi{https://doi.org/10.1073/pnas.1813720116}
"nicabean"
