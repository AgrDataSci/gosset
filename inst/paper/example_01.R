library("gosset")
library("PlackettLuce")
library("climatrends")
library("nasapower")

data("nicabean", package = "gosset")

dat <- nicabean$trial

covar <- nicabean$covar

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

head(R[[1]])

r <- R[[1]]

class(r)

r2 <- unclass(r)

class(r2)

mod <- PlackettLuce(r2)

mod



# kendall 
baseline <- which(grepl("OverallAppreciation", traits))

kendall <- lapply(R[-baseline], function(X){
  kendallTau(x = X, y = R[[baseline]])
})

kendall <- do.call("rbind", kendall)

kendall$trait <- traits[-baseline]

kendall

# worth map

mod <- lapply(R, PlackettLuce)

worth_map(mod[-baseline],
          labels = traits[-baseline], 
          ref = "Amadeus 77") +
  labs(x = "Variety",
       y = "Trait")






