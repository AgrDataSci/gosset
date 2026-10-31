#' Kendall rank correlation coefficient
#' 
#' Compute Kendall rank correlation coefficient between two objects. 
#' Kendall is a coefficient used in statistics to measure the ordinal 
#' association between two measured quantities. A tau test is a non-parametric 
#' hypothesis test for statistical dependence based on the tau coefficient.
#' The 'kendallTau' function applies the "kendall" method from 'stats::cor' 
#' with some previous treatment in the data, such as converting floating numbers
#' into ranks (from the higher being the first and negative being the last) 
#' and the possibility to remove zeros from incomplete ranks
#' 
#' @author Kauê de Sousa and Jacob van Etten
#' @family goodness-of-fit functions
#' @param x a numeric vector, matrix or data frame
#' @param y a vector, matrix or data frame with compatible dimensions to \code{x}
#' @param nboot integer, the size of the bootstrap sample
#' @param null.rm logical, to remove zeros from \code{x} and \code{y} 
#' @param average logical, if \code{FALSE} returns the kendall and N-effective for each entry
#' @param na.omit logical, if \code{TRUE} ignores entries with kendall = NA 
#'  when computing the average
#' @param seed integer, the seed for random number generation. If NULL
#'  (the default), the current random number stream is used
#' @param alternative the alternative hypothesis, one of \code{"two.sided"}
#'  (the default), \code{"greater"} (positive association) or
#'  \code{"less"} (negative association). The p-value is returned in the
#'  column \code{Pr(>|z|)} whatever the alternative
#' @param ... further arguments affecting the Kendall tau produced. See details 
#' @return The Kendall correlation coefficient, the Effective N (the
#' equivalent N if all items were compared to all items), the z value and
#' its p-value. For matrices, each row is treated as an independent
#' ranking: the average tau weights rows by their number of pairs, and the
#' z value uses the null variance of that weighted average. The null
#' variance assumes no ties; use \code{\link{kendallTau_permute}} for an
#' exact test with small or tied data.
#' @references 
#' 
#' Kendall M. G. (1938). Biometrika, 30(1–2), 81–93. 
#' \doi{https://doi.org/10.1093/biomet/30.1-2.81}
#' 
#' @examples
#' 
#' # Vector based example same as stats::cor(x, y, method = "kendall")
#' # but showing N-effective
#' x = c(1, 2, 3, 4, 5)
#' 
#' y = c(1, 1, 3, 2, NA)
#' 
#' w = c(1, 1, 3, 2, 5)
#' 
#' kendallTau(x, y)
#' 
#' kendallTau(x, w)
#' 
#' # Matrix and PlacketLuce ranking example 
#' 
#' library("PlackettLuce")
#'  
#' R = matrix(c(1, 2, 4, 3,
#'              1, 4, 2, 3,
#'              1, 2, NA, 3,
#'              1, 2, 4, 3,
#'              1, 3, 4, 2,
#'              1, 4, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) = LETTERS[1:4]
#' 
#' G = group(as.rankings(R), 1:6)
#' 
#' mod = pltree(G ~ 1, data = G)
#' 
#' preds = predict(mod)
#' 
#' kendallTau(R, preds)
#' 
#' # Also returns raw values (no average) 
#' 
#' kendallTau(R, preds, average = FALSE)
#' 
#' # Choose to ignore entries with NA
#' R2 = matrix(c(1, 2, 4, 3,
#'               1, 4, 2, 3,
#'               NA, NA, NA, NA,
#'               1, 2, 4, 3,
#'               1, 3, 4, 2,
#'               1, 4, 3, 2), nrow = 6, byrow = TRUE)
#' 
#' kendallTau(R, R2, average = FALSE)
#' 
#' kendallTau(R, R2, average = TRUE)
#' 
#' kendallTau(R, R2, average = TRUE, na.omit = TRUE)
#' 
#' # Variability of the average tau across participants
#' boot = kendallTau_bootstrap(R, preds, nboot = 200, seed = 1)
#' quantile(boot, c(0.025, 0.5, 0.975))
#' 
#' @seealso \code{\link[stats]{cor}}
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @importFrom stats cor
#' @importFrom PlackettLuce as.grouped_rankings
#' @export
kendallTau = function(x, y, null.rm = TRUE, average = TRUE, na.omit = FALSE, ...){
  
  UseMethod("kendallTau")
  
}

#' @rdname kendallTau
#' @export
kendallTau.default = function(x, y, null.rm = TRUE, ...,
                             alternative = c("two.sided", "greater", "less")){
  
  alternative = match.arg(alternative)
  
  
  keep = !is.na(x) & !is.na(y)
  
  # if TRUE, remove zeros in both rankings
  if (null.rm) {
    
    keep = x != 0 & y != 0 & keep
    
  }
  
  x = x[keep]
  
  y = y[keep]
  
  # if any decimal in x or y transform it to integer rankings
  # decimals will be computed as descending rankings
  # where the highest values are the "best" 
  # negative values are placed as least positions
  if (any(.is_decimal(x))) {
    
    x = .rank_decimal(x)$rank
    
  }
  
  if(any(.is_decimal(y))) {
    
    y = .rank_decimal(y)$rank
    
  }
  
  tau_cor = stats::cor(x, 
                       y, 
                       method = "kendall", 
                       ...)
  
  n = length(x)
  
  weight = n * (n - 1) / 2
  
  kt = c(tau_cor, weight)
  
  # Extract the values from the vector
  N = kt[2]
  
  # Effective N is the equivalent N needed if all were compared to all
  # N_comparisons = ((N_effective - 1) * N_effective) / 2
  # This is used for significance testing later
  N_effective = 0.5 + sqrt(0.25 + 2 * sum(N)) 
  
  kt[2] = N_effective
  
  n = N_effective
  
  # z-value from the null variance of tau for one ranking of n items
  z = tau_cor / sqrt(.kendall_null_var(n))
  
  p = .kendall_pvalue(z, alternative)
  
  kt = c(kt, z, p)
  
  names(kt) = c("kendallTau", "N_effective", "Zvalue", "Pr(>|z|)")
  
  kt = t(as.data.frame(kt))
  
  kt = as.data.frame(kt)
  
  class(kt) = union("gosset_df", class(kt))
  
  rownames(kt) = 1:nrow(kt)
  
  return(kt)
  
}

#' @rdname kendallTau
#' @method kendallTau matrix
#' @export
kendallTau.matrix = function(x, y, null.rm = TRUE, average = TRUE, na.omit = FALSE, ...,
                            alternative = c("two.sided", "greater", "less")){
  
  alternative = match.arg(alternative)
  
  nc = ncol(x)
  
  kt = apply(cbind(x, y), 1, function(K){
    
    X = K[1:nc]
    Y = K[(nc + 1):(nc * 2)]
    
    kendallTau(X, Y, null.rm = null.rm, alternative = alternative, ...)
    
  })
  
  kt = do.call("rbind", kt)
  
  if (isFALSE(average)) {
    rownames(kt) = 1:nrow(kt)
    return(kt)
  }
  
  if (isTRUE(na.omit)) {
    kt = kt[!is.na(kt[,1]), ]
  } 
  
  # Extract the values from the matrix. Column 2 holds the effective
  # number of items in each row; convert it back to the number of pairs
  # compared, so that rows are weighted by their pairs and the overall
  # effective N is computed from the total number of pairs
  tau = kt[,1]
  n_items = kt[,2]
  N = n_items * (n_items - 1) / 2
  
  tau_average = sum(tau * N, na.rm = TRUE) / sum(N[!is.na(tau)])
  
  # Effective N is the equivalent N needed if all were compared to all
  # N_comparisons = ((N_effective - 1) * N_effective) / 2
  # It is reported for reference only
  ok = !is.na(tau) & n_items >= 2
  N_effective = 0.5 + sqrt(0.25 + 2 * sum(N[ok])) 
  
  # Each row (participant) is an independent comparison. The variance of
  # the pair-weighted mean tau under the null hypothesis is the weighted
  # sum of the null variances of the rows. Treating all pairs as one
  # ranking of N_effective items understated the precision, and the
  # p-values were far too conservative for large tricot trials
  w = N[ok] / sum(N[ok])
  var_average = sum(w^2 * .kendall_null_var(n_items[ok]))
  z = tau_average / sqrt(var_average)
  
  p = .kendall_pvalue(z, alternative)
  
  kt = c(tau_average, N_effective, z, p)
  
  names(kt) = c("kendallTau", "N_effective", "Zvalue", "Pr(>|z|)")
  
  kt = t(as.data.frame(kt))
  
  kt = as.data.frame(kt)
  
  class(kt) = union("gosset_df", class(kt))
  
  rownames(kt) = 1:nrow(kt)
  
  return(kt)
  
}

#' @rdname kendallTau
#' @method kendallTau rankings
#' @export
kendallTau.rankings = function(x, y, ...){
  
  X = x[1:nrow(x), , as.rankings = FALSE]
  
  Y = y[1:nrow(y), , as.rankings = FALSE]
  
  kendallTau(X, Y, ...)
  
}


#' @rdname kendallTau
#' @method kendallTau grouped_rankings
#' @export
kendallTau.grouped_rankings = function(x, y, ...){
  
  X = x[1:length(x), , as.grouped_rankings = FALSE]
  
  Y = y[1:length(y), , as.grouped_rankings = FALSE]
  
  kendallTau(X, Y, ...)
  
}

#' @rdname kendallTau
#' @method kendallTau paircomp
#' @export
kendallTau.paircomp = function(x, y, ...) {
  
  x = PlackettLuce::as.grouped_rankings(x)
  
  X = x[1:length(x), as.grouped_rankings = FALSE]
  
  y = PlackettLuce::as.grouped_rankings(y)
  
  Y = y[1:length(y), as.grouped_rankings = FALSE]
  
  kendallTau(X, Y, ...)
  
}

#' @rdname kendallTau
#' @details
#' \code{kendallTau_bootstrap()} resamples the rows (participants) with
#' replacement \code{nboot} times and returns the pair-weighted average tau
#' of each resample. The spread of these values estimates the sampling
#' variability of the average tau, for example to draw boxplots by trait.
#' Rows with fewer than two ranked items, or with tau = NA, are dropped
#' first. When \code{seed} is given, the resamples are reproducible and the
#' caller's random number stream is left unchanged.
#' @export
kendallTau_bootstrap = function(x, y, nboot = 100, seed = NULL, ...) {
  
  kt = kendallTau(x, y, average = FALSE, ...)
  
  tau = kt[, 1]
  n_items = kt[, 2]
  
  ok = !is.na(tau) & !is.na(n_items) & n_items >= 2
  tau = tau[ok]
  # each row is weighted by its number of pairs, as in kendallTau()
  N = n_items[ok] * (n_items[ok] - 1) / 2
  
  m = length(tau)
  
  if (m < 2) {
    stop("kendallTau_bootstrap() needs at least two rankings ",
         "with two or more items each")
  }
  
  if (!is.null(seed)) {
    # reproducible resamples, without changing the caller's
    # random number stream
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      old_seed = get(".Random.seed", envir = globalenv(), inherits = FALSE)
      on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
    } else {
      on.exit(if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      }, add = TRUE)
    }
    set.seed(seed)
  }
  
  vapply(seq_len(nboot), function(b) {
    i = sample.int(m, m, replace = TRUE)
    sum(tau[i] * N[i]) / sum(N[i])
  }, numeric(1))
  
}


#' Null variance of Kendall's tau for one ranking of n items without ties
#' @noRd
.kendall_null_var = function(n) {
  2 * (2 * n + 5) / (9 * n * (n - 1))
}

#' P-value from a z statistic
#' @noRd
.kendall_pvalue = function(z, alternative = "two.sided") {
  switch(alternative,
         two.sided = 2 * stats::pnorm(-abs(z)),
         greater = stats::pnorm(z, lower.tail = FALSE),
         less = stats::pnorm(z))
}
