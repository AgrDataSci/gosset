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
#'   \item{n}{Number of sets with two or more ranked items in which the
#'   item appears.}
#'   \item{top}{Expected top probability (\%).}
#'   \item{bottom}{Expected bottom probability (\%).}
#'   \item{net_winning}{Net winning probability (\%).}
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
#' R = matrix(c(1, 2, 0, 0,
#'              4, 1, 2, 3,
#'              2, 1, 3, 0,
#'              1, 2, 3, 0,
#'              2, 1, 3, 0,
#'              1, 0, 3, 2),
#'            nrow = 6, byrow = TRUE)
#' colnames(R) = c("apple", "banana", "orange", "pear")
#' R = as.rankings(R)
#'
#' preference_index(R)
#'
#'@importFrom stats aggregate
#'@importFrom stats coef
#'@export
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
  
  # Exact probability of being last, using subset dynamic programming.
  # The original implementation enumerated all (m-1)! permutations.
  # This recursion caches each remaining subset and has at most 2^(m-1)
  # states. It preserves the exact Plackett-Luce estimand.
  p_top = function(i, others) {
    wi = worth[[i]]
    wi / (wi + sum(worth[others]))
  }
  p_bottom = function(i, others) {
    if (length(others) == 0L) return(1)
    wi = worth[[i]]
    w = unname(worth[others])
    memo = new.env(parent = emptyenv(), hash = TRUE)
    last_probability = function(remaining) {
      if (length(remaining) == 0L) return(1)
      key = paste(remaining, collapse = ",")
      if (exists(key, envir = memo, inherits = FALSE)) {
        return(get(key, envir = memo, inherits = FALSE))
      }
      denom = wi + sum(w[remaining])
      value = sum(vapply(seq_along(remaining), function(j) {
        selected = remaining[j]
        w[selected] / denom * last_probability(remaining[-j])
      }, numeric(1)))
      assign(key, value, envir = memo)
      value
    }
    last_probability(seq_along(others))
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
    # a set with a single item is not a comparison
    if (length(items) < 2L) next
    for (it in items) {
      others = setdiff(items, it)
      # one row per (set, item). Storing 'others' as a column made
      # as.data.frame() recycle each record once per other item, which
      # inflated n and weighted larger sets more heavily in the means
      recs[[idx]] = data.frame(item = it,
                               P_top = p_top(it, others),
                               P_bottom = p_bottom(it, others),
                               stringsAsFactors = FALSE)
      idx = idx + 1
    }
  }
  
  if (length(recs) == 0L) {
    stop("object must contain at least one set with two or more ranked items")
  }
  
  df = do.call(rbind, recs)
  
  # aggregate
  agg = stats::aggregate(cbind(P_top, P_bottom) ~ item, data = df, FUN = mean)
  
  n = as.data.frame(table(df$item)); names(n) = c("item", "n")
  
  out = merge(n, agg, by = "item", sort = FALSE)
  
  
  out$top = 100 * out$P_top
  out$bottom = 100 * out$P_bottom
  out$net_winning = out$top - out$bottom
  out$worth = as.numeric(worth[out$item])
  out$P_top = out$P_bottom = NULL
  
  # order by net winning probs (descending, like a leaderboard)
  out = out[order(-out$net_winning), ]
  
  rownames(out) = NULL
  
  class(out) = union("gosset_df", class(out))
  
  out
  
}
