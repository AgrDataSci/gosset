#' Common bean on-farm trial in Nicaragua
#'
#' Data from decentralized on-farm trials of common bean 
#' (\emph{Phaseolus vulgaris} L.) varieties in Nicaragua over the 
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
#'  \code{nicabean[["bean_covar"]]} contains the covariates associated with the data
#'  Codes for contests are described:
#'  \describe{
#'  \item{\code{id}}{the trial id}
#'  \item{\code{adm0}}{the country name where trials were set}
#'  \item{\code{longitude}}{the longitude of the trial plot}
#'  \item{\code{latitude}}{the latitude of the trial plot}
#'  \item{\code{trial}}{the trial name as registered on ClimMob}
#'  \code{nicabean[["bean_rank"]]} contains the trial data.
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
