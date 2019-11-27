#' Akaike weights
#' 
#' Akaike weights represent the relative likelihood of a model. 
#' It can be used in model averaging and selection.
#' 
#' @param object a numerical vector with models goodness of fit coefficients 
#' @return A data frame containing the coefficients:
#' \item{delta}{the delta overall change in the coefficients}
#' \item{relative_logLik}{the relative log-likelihood}
#' \item{akaike_weights}{the Akaike weights}
#' @examples
#' 
#' data("airquality")
#' 
#' # try three model approaches
#' mod1 <- glm(Temp ~ 1,
#'             data = airquality,
#'             family = poisson())
#' 
#' mod2 <- glm(Temp ~ Ozone,
#'             data = airquality,
#'             family = poisson())
#' 
#' mod3 <- glm(Temp ~ Ozone + Solar.R,
#'             data = airquality,
#'             family = poisson())
#' 
#' 
#' akaike_weights(c(AIC(mod1), AIC(mod2), AIC(mod3)))
#'  
#' @export 
akaike_weights <- function(object){

  # take the delta value
  delta <- object - min(object, na.rm = TRUE)
  
  # and the relative log likelihood
  rel_LL <- exp(-0.5 * delta)
  
  # sum all relative log likelihoods
  sum_LL <- sum(rel_LL, na.rm = TRUE)
  
  # get the Akaike weight
  weights <- rel_LL / sum_LL

  # combine values into a single vector
  result <- tibble::tibble(delta = delta, 
                           relative_logLik = rel_LL, 
                           akaike_weights = weights)
  
  return(result)
  
}
