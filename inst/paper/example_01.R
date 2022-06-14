library("PlackettLuce")
library("gosset")
library("climatrends")
library("nasapower")

data("nicabean", package = "gosset")

dat <- nicabean$bean_rank

covar <- nicabean$bean_covar


traits <- unique(dat$trait)

R <- list()

for (i in seq_along(traits)) {
  dat_i <- subset(dat, dat$trait == traits[i])
  R[[i]] <- rank_numeric(data = dat_i,
                         items = "item",
                         input = "rank", 
                         id = "id", 
                         ascending = TRUE)
}