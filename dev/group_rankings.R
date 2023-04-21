library("PlackettLuce")
library("ggplot2")

# In this approach we combine all the rankings across the trial 
# into a single grouped_rankings using the function group() which 
# puts the rankings together using the argument index = 
# in this case the index is the package id, this can be used even when
# participants don't evaluate all the traits and could be used as an 
# implementation for variety performance across the trial considering 
# all the traits, not only overall performance

# Here I show a combination of three traits + overall performance
# simulating a real trial when not all the traits are assessed by the participant

# marketability
R1 <- matrix(c(1, 4, 2, 3,
               4, 1, 2, 3,
               2, 1, 3, 4,
               1, 2, 3, 0,
               2, 1, 3, 4,
               1, 4, 3, 2), nrow = 6, byrow = TRUE)
colnames(R1) <- c("apple", "banana", "orange", "pear")

R1 <- as.rankings(R1)

R1

i1 <- c(1:6) 

# yield
R2 <- matrix(c(1, 2, 0, 0,
               3, 1, 2, 4,
               2, 1, 3, 4,
               2, 4, 3, 1,
               3, 1, 4, 2,
               1, 4, 3, 2), nrow = 6, byrow = TRUE)
colnames(R2) <- c("apple", "banana", "orange", "pear")

R2 <- as.rankings(R2)

i2 <- c(1:6) 

# pest resistance
R3 <- matrix(c(4, 1, 2, 3,
               4, 1, 3, 2,
               3, 1, 2, 4,
               1, 2, 3, 4,
               3, 4, 1, 2), nrow = 5, byrow = TRUE)
colnames(R3) <- c("apple", "banana", "orange", "pear")

R3 <- as.rankings(R3)

i3 <- c(1,2,4,5,6)

# diseases resistance
R4 <- matrix(c(2, 1, 3, 4,
               2, 1, 3, 4,
               2, 1, 3, 4), nrow = 3, byrow = TRUE)
colnames(R4) <- c("apple", "banana", "orange", "pear")

R4 <- as.rankings(R4)

i4 <- c(2,4,6)

# as list because is easy to scale in a loop 
R <- list(R1, R2, R3, R4)
index <- list(i1, i2, i3, i4)

# rbind the rankings
R <- do.call("rbind", R)

# and the indices (tricot packages)
index <- unlist(index)

# with the indices we can combine rankings 
# even if they are not complete over the trial
G <- group(R, index = index)

G

# check the rankings for the first index 
g <- (G[1])

g[1:length(G), , as.grouped_rankings = F]

# this can be used for the prioritized traits in each crop 
# to help in assessing the best genotypes considering trait 
# performance
# maybe it can generate even better trees with socio-economic 
# or environmental covariates


# another example is by adding weights
nrankers <- 6
nchars <- 4

w <- as.numeric(table(index) / nchars)

w

# put the coefficients together and plot it to see 
# the differences in worth
coef1 <- coefficients(PlackettLuce(R1))
coef2 <- coefficients(PlackettLuce(R2))
coef3 <- coefficients(PlackettLuce(R3))
coef4 <- coefficients(PlackettLuce(R4))
coef5 <- coefficients(PlackettLuce(G))
coef6 <- coefficients(PlackettLuce(G, weights = w))


dat <- data.frame(logworth = c(coef1, coef2, coef3, coef4, coef5, coef6),
                  item = names(c(coef1, coef2, coef3, coef4, coef5, coef6)),
                  model = rep(c("Marketability", 
                                "Yield", 
                                "Pests", 
                                "Diseases",
                                "G",
                                "G-weighted"), each = 4))


dat$model <- factor(dat$model, levels = c("Marketability", 
                                          "Yield", 
                                          "Pests", 
                                          "Diseases",
                                          "G",
                                          "G-weighted"))

# plot 
ggplot(dat, aes(x = logworth,
                y = item, 
                group = model, 
                color = model)) + 
  geom_point(size = 3) + 
  theme_classic() +
  labs(x = "Log-worth",
       y = "Item")

