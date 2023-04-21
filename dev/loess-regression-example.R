# LOESS regression with Plackett-Luce model

library(PlackettLuce)
library(ggplot2)

# Bean data from PL package
data(beans)
beans$middle <- complete(beans[c("best", "worst")],
                         items = c("A", "B", "C"))
varieties <- beans[c("variety_a", "variety_b", "variety_c")]
order3 <- decode(beans[c("best", "middle", "worst")],
                 items = beans[c("variety_a", "variety_b", "variety_c")],
                 code = c("A", "B", "C"))
outcome <- unlist(beans[c("var_a", "var_b", "var_c")])
trial_variety <- unlist(beans[c("variety_a", "variety_b", "variety_c")])
order2 <- data.frame(Winner = ifelse(outcome == "Worse",
                                     "Local", trial_variety),
                     Loser = ifelse(outcome == "Worse",
                                    trial_variety, "Local"),
                     stringsAsFactors = FALSE, row.names = NULL)

R <- rbind(as.rankings(order3, input = "orderings"),
           as.rankings(order2, input = "orderings"))

R

# Inputs
n <- 100 # number of points to calculate
d <- 3 # strength of distance decay for case weights in LOESS function
n_r <- dim(R)[1]
n_v <- dim(R)[2]

# Order the rankings
interval0to1 <- seq(from = 0 + 1/n, to = 1 - 1/n, length.out = n) # avoid zero distance values
maxTN <- c(beans$maxTN, rep(beans$maxTN, each=3))
maxTNrescaled <- (maxTN - min(maxTN)) / (max(maxTN) - min(maxTN))
cc <- (max(maxTN) - min(maxTN)) / n
intervalMintoMax <-  seq(from = min(maxTN) + cc, to = max(maxTN) - cc, length.out = n)

# Prepare the matrix for the results
worth_maxTN <- matrix(NA, nrow=n, ncol=n_v)

for(i in 1:n){
  
  weights <- (1 - (abs(interval0to1[i] - maxTNrescaled)^d))^d
  weights <- (weights / sum(weights)) * length(weights)
  worths <- qvcalc(PlackettLuce(R, weights=weights))[["qvframe"]]
  worth_maxTN[i,] <- exp(worths[,1]) / sum(exp(worths[,1]))
  
}

plot(weights, maxTN)

data1 <- data.frame(maxNT = rep(intervalMintoMax, n_v),                    # Create data frame 
                   worth = as.vector(worth_maxTN),
                   variety = rep(rownames(worths), each=n))

ggplot(data1, aes(x = maxNT, y = worth, col = variety)) +           # Draw line plot with ggplot2
  geom_line()
