# Load necessary library
library(stats)

# Example data
data <- data.frame(
  group = c(rep("Group1", 50), rep("Group2", 50)),
  trait = rnorm(100),  # Replace with actual trait values
  overall = rnorm(100) # Replace with actual "Overall" values
)

# Function to calculate Kendall's Tau for a group
calculate_tau <- function(group_data) {
  cor(group_data$trait, group_data$overall, method = "kendall")
}

# Observed Kendall's Tau for each group
tau_group1 <- calculate_tau(data[data$group == "Group1", ])
tau_group2 <- calculate_tau(data[data$group == "Group2", ])
observed_diff <- abs(tau_group1 - tau_group2)

# Permutation test
set.seed(123)  # For reproducibility
n_permutations <- 1000
permuted_diffs <- numeric(n_permutations)

for (i in 1:n_permutations) {
  # Shuffle group labels
  shuffled_data <- data
  shuffled_data$group <- sample(shuffled_data$group)
  
  # Recalculate Kendall's Tau for permuted groups
  tau_perm_group1 <- calculate_tau(shuffled_data[shuffled_data$group == "Group1", ])
  tau_perm_group2 <- calculate_tau(shuffled_data[shuffled_data$group == "Group2", ])
  
  # Calculate absolute difference in permuted Kendall's Tau
  permuted_diffs[i] <- abs(tau_perm_group1 - tau_perm_group2)
}

# P-value: Proportion of permuted differences greater than or equal to observed difference
p_value <- mean(permuted_diffs >= observed_diff)

# Output results
cat("Observed Difference in Kendall's Tau:", observed_diff, "\n")
cat("P-value from Permutation Test:", p_value, "\n")

# Visualize permutation test results
hist(permuted_diffs, breaks = 30, main = "Permutation Test Results",
     xlab = "Difference in Kendall's Tau")
abline(v = observed_diff, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Observed Difference"), col = "red", lwd = 2, lty = 2)
