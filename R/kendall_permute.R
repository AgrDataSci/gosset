#' Kendall's tau permutation test
#' 
#' Perform a pairwise permutation test to assess statistical differences 
#' in Kendall's Tau correlation between two or more groups.
#' 
#' @author Kauê de Sousa
#' @inheritParams kendallTau
#' @param split a vector indicating the splitting rule for the test
#' @param n.permutations an integer, the number of permutations to perform
#' @return A data.frame containing:
#' \item{observed_diff}{observed absolute differences in Kendall's tau for all group pairs.}
#' \item{p_values}{p-values from the permutation test for all group pairs.}
#' @seealso \code{\link[gosset]{kendallTau}}
#' @examplesIf interactive()
#' set.seed(42)
#' x = rnorm(100)
#' y = rnorm(100)
#' split = rep(c("Group1", "Group2", "Group3"), length.out = 100)
#' kendall_permute(x, y, split)
#' 
#' data("breadwheat", package = "gosset")
#' 
#' x = rank_tricot(breadwheat, 
#'                 items = paste0("variety_", letters[1:3]),
#'                 input = c("yield_best", "yield_worst"),
#'                 validate.rankings = TRUE)
#' 
#' y = rank_tricot(breadwheat, 
#'                 items = paste0("variety_", letters[1:3]),
#'                 input = c("overall_best", "overall_worst"),
#'                 validate.rankings = TRUE)
#'                 
#' kendall_permute(x, y, 
#'                 split = rep(c("Group1", "Group2", "Group3"), length.out = nrow(breadwheat)), 
#'                 n.permutations = 100)
#'                 
#' @importFrom parallel mclapply
#' @export
kendall_permute = function(x, y, split, n.permutations = 500) {
  # Ensure inputs are of equal length
  if (length(x) != length(y) || length(x) != length(split)) {
    stop("x, y, and split must have the same length.")
  }
  
  # Helper function to calculate Kendall's Tau for a group
  calculate_tau = function(group_data) {
    kendallTau(group_data$x, group_data$y)$kendallTau
  }
  
  # Combine x, y, and split into a single data frame
  combined_data = data.frame(x = x, y = y, group = split)
  group_names = unique(split)
  n_groups = length(group_names)
  
  if (n_groups < 2) {
    stop("The 'split' vector must divide data into at least two groups.")
  }
  
  # Calculate observed Kendall's Tau for each group
  taus = sapply(group_names, function(group) {
    calculate_tau(combined_data[combined_data$group == group, ])
  })
  
  # Pairwise group comparisons
  group_pairs = combn(group_names, 2, simplify = FALSE)
  
  # Pre-generate permutations
  permuted_splits = replicate(n.permutations, sample(combined_data$group), simplify = FALSE)
  
  # Function to compute permutation differences
  compute_permuted_diff = function(pair) {
    group1 = pair[1]
    group2 = pair[2]
    
    # Observed difference
    observed_diff = abs(taus[group1] - taus[group2])
    
    # Permuted differences
    permuted_diffs = sapply(permuted_splits, function(perm_split) {
      combined_data$group = perm_split
      tau1 = calculate_tau(combined_data[combined_data$group == group1, ])
      tau2 = calculate_tau(combined_data[combined_data$group == group2, ])
      abs(tau1 - tau2)
    })
    
    # P-value
    p_value = mean(permuted_diffs >= observed_diff)
    
    list(observed_diff = observed_diff, p_value = p_value)
  }
  
  # Parallelize permutation computation
  results_list = parallel::mclapply(group_pairs, compute_permuted_diff, mc.cores = detectCores() - 1)
  
  # Combine results into a data frame
  results = do.call(rbind, lapply(seq_along(group_pairs), function(i) {
    data.frame(
      group1 = group_pairs[[i]][1],
      group2 = group_pairs[[i]][2],
      observed_diff = results_list[[i]]$observed_diff,
      p_value = results_list[[i]]$p_value,
      stringsAsFactors = FALSE
    )
  }))
  
  rownames(results) = 1:nrow(results)
  class(results) = union("gosset_df", class(results))
  
  return(results)
}

