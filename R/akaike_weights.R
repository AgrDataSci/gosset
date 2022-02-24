#' Akaike weights
#' 
#' Akaike weights represent the relative likelihood of a model.  
#' It can be used in model averaging and selection.
#' 
#' @author Kauê de Sousa and Jacob van Etten
#' @param object a numerical vector with models goodness of fit coefficients 
#' @return A data frame containing the coefficients:
#' \item{delta}{the delta overall change in the coefficients}
#' \item{relative_logLik}{the relative log-likelihood}
#' \item{akaike_weights}{the Akaike weights}
#' @references 
#' 
#' Wagenmakers E. J. & Farrell S. (2004). 
#' Psychonomic Bulletin and Review, 11(1), 192–196. 
#' \doi{https://doi.org/10.3758/BF03206482}
#' 
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
#' # models AICs together in a single vector
#' models <- c(mod1 = AIC(mod1),
#'             mod2 = AIC(mod2),
#'             mod3 = AIC(mod3))
#' 
#' # calculate akaike weights
#' aw <- akaike_weights(models)
#' 
#' # the higher the better
#' names(models[which.max(aw$akaike_weights)])
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

  # combine values into a single data.frame
  result <- data.frame(delta = delta, 
                       relative_logLik = rel_LL, 
                       akaike_weights = weights)
  
  class(result) <- union("gosset_df", class(result))
  
  return(result)
  
}
