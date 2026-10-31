#' Likelihood-ratio test
#' 
#' Assesses the goodness of fit of competing
#'  statistical models
#' 
#' @param x an object of class rankings or grouped_rankings
#' @param object an object of class PlackettLuce
#' @param split a vector indicating the splitting rule for the test
#' @param ... additional arguments passed to methods
#' @author Joost van Heerwaarden and Kauê de Sousa
#' @examples 
#' library("PlackettLuce")
#' example("beans", package = "PlackettLuce")
#' G = group(R, rep(seq_len(nrow(beans)), 4))
#' d = cbind(G, beans)
#' 
#' split = ifelse(d$maxTN < 18.7175, TRUE, FALSE)
#' 
#' likelihood_ratio(G, split)
#' 
#' mod = PlackettLuce(G)
#' 
#' anova(mod)
#' 
#' @importFrom stats pchisq
#' @importFrom PlackettLuce PlackettLuce
#' @export
likelihood_ratio = function(x, split, ...) {
  
  if (length(split) != nrow(x) || anyNA(split)) {
    stop('split must have one non-missing value per ranking observation')
  }
  if (length(unique(split)) < 2L) {
    stop('split must define at least two non-empty groups')
  }

  # fit model with all data
  PL_all = PlackettLuce::PlackettLuce(x, ...)
  
  # iterate over splits, estimate worth, and store models in list
  DF_resid_sum = 0 
  LL_sum = 0 
  
  # split the data 
  x_split = split(x, split)

  for (i in seq_along(x_split)) {
    #fit the model
    PL_model_lev  =  PlackettLuce::PlackettLuce(x_split[[i]], ...)
    LL_sum = LL_sum + PL_model_lev$loglik
    DF_resid_sum = DF_resid_sum + PL_model_lev$df.residual
  }
  
  DF_delta = PL_all$df.residual - DF_resid_sum
  Deviance = -2 * (PL_all$loglik - LL_sum)
  
  if (!is.finite(Deviance) || !is.finite(DF_delta) || DF_delta <= 0) {
    stop('Likelihood-ratio comparison has invalid deviance or degrees of freedom')
  }
  # Chisq P value
  p_val_chisq = stats::pchisq(Deviance, DF_delta, lower.tail = FALSE)
  
  stars = .stars_pval(p_val_chisq)[1]
  
  out = data.frame("deviance" = Deviance, 
                   "DF_delta" = DF_delta, 
                   "Pr(>Chisq)" = p_val_chisq,
                   " " = stars,
                   check.names = FALSE,
                   stringsAsFactors = FALSE)
  
  class(out) = union("gosset_df", class(out))
  
  return(out)
  
}

