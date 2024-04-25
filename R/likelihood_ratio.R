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
  
  # fit model with all data
  PL_all = PlackettLuce::PlackettLuce(x)
  
  # iterate over splits, estimate worth, and store models in list
  PL_model_list = c()
  DF_resid_sum = 0 
  LL_sum = 0 
  
  # split the data 
  x_split = split(x, split)

  for(i in 1:length(x_split)){
    #fit the model
    PL_model_lev  =  PlackettLuce::PlackettLuce(x_split[[i]])
    LL_sum = LL_sum + PL_model_lev$loglik
    DF_resid_sum = DF_resid_sum + PL_model_lev$df.residual
  }
  
  DF_delta = PL_all$df.residual - DF_resid_sum
  Deviance = -2 * (PL_all$loglik - LL_sum)
  
  # Chisq P value
  p_val_chisq  =  1 - stats::pchisq(Deviance, DF_delta)
  
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

