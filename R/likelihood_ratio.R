#' Likelihood-ratio test
#' 
#' Assesses the goodness of fit of competing
#'  statistical models
#' 
#' @param x an object of class rankings or grouped_rankings
#' @param object an object of class PlackettLuce
#' @param split a vector indicating the splitting rule for the test
#' @param ... additional arguments passed to methods
#' @details
#' The test compares a Plackett-Luce model fitted to all data with
#'  separate models fitted within each group defined by \code{split}.
#'  The degrees of freedom are the sum of the ranks (number of estimated
#'  parameters) of the group models minus the rank of the global model.
#'  When a group has no ties, its tie parameter is fixed at zero, at the
#'  boundary of the parameter space, and the chi-square p-value is approximate.
#' @return A data.frame with the deviance, the degrees of freedom
#'  (\code{DF_delta}) and the p-value of the chi-square test
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
  
  # NROW() covers both rankings (a matrix) and grouped_rankings (a vector
  # of group indices, for which nrow() is NULL)
  if (length(split) != NROW(x) || anyNA(split)) {
    stop('split must have one non-missing value per ranking observation')
  }
  if (length(unique(split)) < 2L) {
    stop('split must define at least two non-empty groups')
  }

  # fit model with all data
  PL_all = PlackettLuce::PlackettLuce(x, ...)
  
  # iterate over splits, estimate worth, and sum log-likelihoods and ranks
  rank_sum = 0
  LL_sum = 0

  # split the data
  x_split = split(x, split)

  for (i in seq_along(x_split)) {
    #fit the model
    PL_model_lev  =  PlackettLuce::PlackettLuce(x_split[[i]], ...)
    LL_sum = LL_sum + PL_model_lev$loglik
    rank_sum = rank_sum + PL_model_lev$rank
  }

  # degrees of freedom are the extra parameters estimated by the split
  # models. The difference in residual df is not used, because PlackettLuce
  # counts residual df from the possible selections in each choice set,
  # which depend on the largest tie order observed in the data. When groups
  # differ in tie order, residual df are not additive across groups
  DF_delta = rank_sum - PL_all$rank
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

