
library("PlackettLuce")
library("gosset")
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

data("nicabean", package = "gosset")

dat = nicabean$trial

traits = unique(dat$trait)
items = unique(dat$item)
n = length(unique(dat$id))

# a list to keep all the rankings 
R = list()

# a list to keep the indices (ids), this can work 
# even when an id is missing for a certain trait
indices = list()

indices_levels = unique(unique(dat$id))

for (i in seq_along(traits)) {
  
  dat_i = subset(dat, dat$trait == traits[i])
  
  R[[i]] = rank_numeric(data = dat_i,
                        items = "item",
                        input = "rank", 
                        id = "id", 
                        ascending = TRUE)
  
  indices[[i]] = unique(dat_i$id)
  
}

# choose some traits related to yield and market 
G = matrix(NA, ncol = length(items), nrow = 1, dimnames = list(1, items))

for (i in seq_along(R)) {
  G = rbind(G, R[[i]])
}

G = G[-1, ]

# unlist the indices
indices = unlist(indices)
# rescale indices to start with 1
# so group() will used it as an index in the rows in G
indices = as.integer(factor(indices, levels = indices_levels))

G = group(G, index = indices)

# check the first grouped ranking
g = G[1, ]

g[1:length(G), , as.grouped_rankings = F]

coeffsG = coefficients(PlackettLuce(G))

# this can be used for the prioritized traits in each crop 
# to help in assessing the best genotypes considering trait 
# performance

# another example is by adding weights, we consider R as a full rankings
Rw = matrix(NA, ncol = length(items), nrow = 1, dimnames = list(1, items))

for (i in seq_along(R)) {
  Rw = rbind(Rw, R[[i]])
}

Rw = Rw[-1, ]

# use kendall tau as a type og weighted
kendall = lapply(R[-9], function(x){
  kendallTau(R[[9]], x)
})

kendall = do.call("rbind", kendall)

weights = c(kendall$kendallTau, 1)

weights = rep(weights, each = n)

plot(weights)

coeffsRw = coefficients(PlackettLuce(Rw, weights = weights))

# put the coefficients together and plot it to see 
coeffs = vector()

for(i in seq_along(R)){
  coeffs = c(coeffs, coefficients(PlackettLuce(R[[i]])))
}

# add the coeffs from the other models 
coeffs = c(coeffs, coeffsG, coeffsRw)

dat = data.frame(logworth = coeffs,
                 item = rep(items, times = length(traits) + 2),
                 model = rep(c(traits, 
                               "Grouped", 
                               "R weighted"),
                             each = length(items)))


dat$model = factor(dat$model, 
                   levels = c(traits, 
                              "Grouped", 
                              "R weighted"))

# plot 
ggplot(dat, aes(x = logworth,
                y = item, 
                group = model, 
                shape = model)) + 
  geom_point(size = 2) +
  scale_shape_manual(values = 1:length(unique(dat$model))) +
  theme_classic() +
  labs(x = "Log-worth",
       y = "Item")

