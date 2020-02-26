#' Agricultural and livelihood practices identified by Kenyan farmers
#' 
#' Farmers were asked to rank 9 different household-level practices 
#'  according to their interest in receiving more information about them.
#'  Through a simple ranking experiment, we then determined farmers' 
#'  individual preferences for information about 9 alternative information
#'  options \code{kenyachoice[["contests"]]}. Household socioeconomic data was 
#'  collected to link farmers' preferences with explanatory data 
#'  \code{kenyachoice[["predictors"]]}.
#'  
#' @keywords dataset 
#' @format A list with two dataframes. \code{kenyachoice[["contests"]]} contains 
#'  the pairwise rankings from farmers choices. \code{kenyachoice[["predictors"]]}
#'  contains the socioeconomic data for each farmer.
#'  Codes for contests are described:
#'  \describe{
#'  \item{\code{B}}{Opening a business}
#'  \item{\code{D}}{Dry planting}
#'  \item{\code{G}}{Collective crop marketing}
#'  \item{\code{J}}{Finding off-farm job}
#'  \item{\code{M}}{Machine tillage}
#'  \item{\code{O}}{Renting out traction animals}
#'  \item{\code{R}}{Mulching}
#'  \item{\code{T}}{Terracing}
#'  \item{\code{Z}}{Zai pits}  
#'  }
#' @source 
#' Steinke, J., et. al. (2019). Household-specific targeting of agricultural advice 
#' via mobile phones: Feasibility of a minimum data approach for smallholder context. 
#' Computers and Electronics in Agriculture, 162, 991–1000. 
#' \url{https://doi.org/10.1016/j.compag.2019.05.026}
"kenyachoice"
