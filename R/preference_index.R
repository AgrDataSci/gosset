#' Preference index from rankings 
#'
#' Computes a preference index for each item in a ranking 
#' using Plackett–Luce worth parameters. For each item, the function
#' estimates the probability of being ranked at the top or bottom
#' of the sets in which it appears, and calculates a net winning
#' probability as the difference between top and bottom probabilities.
#'
#' @param object A \code{rankings} object (from \pkg{PlackettLuce}) or
#' a matrix that can be coerced to rankings with
#' \code{\link[PlackettLuce]{as.rankings}}.
#'
#' @details
#' The preference index provides a model-based summary of performance
#' across all ranking sets. Probabilities are derived from the fitted
#' Plackett–Luce worth parameters, normalized to sum to one.
#'
#' For each item:
#' \itemize{
#'   \item \code{top_probs} is the expected probability (in percent)
#'   of being ranked first in its sets.
#'   \item \code{bottom_probs} is the expected probability (in percent)
#'   of being ranked last in its sets.
#'   \item \code{net_winning_probs} is the difference
#'   \code{top_probs - bottom_probs}, a relative preference score.
#'   \item \code{worth} is the normalized worth parameter from the
#'   Plackett–Luce model.
#' }
#'
#' The output is ordered by descending net winning probability,
#' providing a leaderboard-style summary of items.
#'
#' @return A \code{gosset_df} (data.frame subclass) with columns:
#' \describe{
#'   \item{item}{Item identifier (name).}
#'   \item{n}{Number of appearances of the item across sets.}
#'   \item{top_probs}{Expected top probability (\%).}
#'   \item{bottom_probs}{Expected bottom probability (\%).}
#'   \item{net_winning_probs}{Net winning probability (\%).}
#'   \item{worth}{Normalized worth parameter.}
#' }
#'
#' @seealso \code{\link[PlackettLuce]{PlackettLuce}},
#' \code{\link[PlackettLuce]{as.rankings}},
#' \code{\link[gosset]{worth_map}}
#'
#' @examples
#' library(PlackettLuce)
#'
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 1, 3, 0,
#'               1, 2, 3, 0,
#'               2, 1, 3, 0,
#'               1, 0, 3, 2),
#'             nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' R <- as.rankings(R)
#'
#' preference_index(R)
#'
#' @export

preference_index = function(object) {
  
  # convert to rankings if needed
  if (!inherits(object, "rankings")) {
    R = PlackettLuce::as.rankings(object)
  } else {
    R = object
  }
  item_names = colnames(R)
  if (is.null(item_names)) item_names = paste0("item", seq_len(ncol(R)))
  colnames(R) = item_names
  
  fit   = PlackettLuce::PlackettLuce(R)
  worth = stats::coef(fit, log = FALSE)
  # normalize (optional but convenient)
  worth = worth / sum(worth)  
  
  # simple permutations (no extra packages)
  perms = function(x) {
    n = length(x)
    if (n <= 1) return(list(x))
    out = list()
    k = 1
    for (i in seq_len(n)) {
      for (p in perms(x[-i])) {
        out[[k]] = c(x[i], p); k = k + 1
      }
    }
    out
  }
  p_top = function(i, others) {
    wi = worth[[i]]
    wi / (wi + sum(worth[others]))
  }
  # general PL probability that i is last among {i} ∪ others
  p_bottom = function(i, others) {
    m = length(others)
    if (m == 1) return(1 - p_top(i, others))   # 2 items total
    total = 0
    for (ord in perms(others)) {
      # sequentially choose top among "others" at each stage; i survives to the end
      denom = worth[[i]] + sum(worth[ord])
      prod_term = 1
      remaining = ord
      for (t in seq_len(m)) {
        jt = remaining[1]
        prod_term = prod_term * (worth[[jt]] / denom)
        denom = denom - worth[[jt]]
        remaining = remaining[-1]
      }
      total = total + prod_term
    }
    total
  }
  
  # build appearances (who appears with whom in each row) ---
  nr = nrow(R)
  present_list = lapply(seq_len(nr), function(r) {
    # items present in this ranking row (rank > 0)
    pres = which(!is.na(R[r, ]) & R[r, ] > 0)
    item_names[pres]
  })
  
  # long records: one per (row, item) with its "others"
  recs = list()
  idx = 1
  for (r in seq_len(nr)) {
    items = present_list[[r]]
    for (it in items) {
      others = setdiff(items, it)
      recs[[idx]] = list(item = it,
                         others = others,
                         P_top = p_top(it, others),
                         P_bottom = p_bottom(it, others))
      idx = idx + 1
    }
  }
  
  df = do.call(rbind, lapply(recs, as.data.frame))
  
  # aggregate
  agg = aggregate(cbind(P_top, P_bottom) ~ item, data = df, FUN = mean)
  
  N = as.data.frame(table(df$item)); names(N) = c("item", "N")
  
  out = merge(N, agg, by = "item", sort = FALSE)
  
  
  out$top_probs = 100 * out$P_top
  out$bottom_probs = 100 * out$P_bottom
  out$net_winning_probs = out$top_probs - out$bottom_probs
  out$worth = as.numeric(worth[out$item])
  out$P_top = out$P_bottom = NULL
  
  # order by net winning probs (descending, like a leaderboard)
  out = out[order(-out$net_winning_probs), ]
  
  rownames(out) = NULL
  
  class(out) = union("gosset_df", class(out))
  
  out
  
}
