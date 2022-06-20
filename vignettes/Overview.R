## ----fetch, message=FALSE, eval=TRUE, echo=TRUE-------------------------------
library("gosset")
library("PlackettLuce")
library("climatrends")
library("nasapower")
library("ggplot2")

data("nicabean", package = "gosset")

dat <- nicabean$trial

covar <- nicabean$covar

lapply(nicabean, head)


## ----rank, message=FALSE, eval=TRUE, echo=TRUE--------------------------------

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


## ----kendall, message=FALSE, eval=TRUE, echo=TRUE-----------------------------
baseline <- which(grepl("OverallAppreciation", traits))

kendall <- lapply(R[-baseline], function(X){
  kendallTau(x = X, y = R[[baseline]])
})

kendall <- do.call("rbind", kendall)

kendall$trait <- traits[-baseline]

print(kendall)

## ----worth, message=FALSE, eval=TRUE, echo=TRUE-------------------------------

mod <- lapply(R, PlackettLuce)

worth_map(mod[-baseline],
          labels = traits[-baseline], 
          ref = "Amadeus 77") +
  labs(x = "Variety",
       y = "Trait")


## ---- message=FALSE, eval=FALSE, echo=TRUE------------------------------------
#  temp <- temperature(dat[, c("lon","lat")],
#                      day.one = dat[, "planting_date"],
#                      span = 80)

## ---- message=FALSE, eval=FALSE, echo=TRUE------------------------------------
#  R <- rank_tricot(dat,
#                   items = c("variety_a","variety_b","variety_c"),
#                   input = c("overall_best","overall_worst"),
#                   group = TRUE)
#  
#  pld <- cbind(R, temp)
#  
#  pl <- pltree(R ~ maxNT + maxDT,
#               alpha = 0.1,
#               gamma = TRUE,
#               data = pld)

## ---- message=FALSE, eval=FALSE, echo=TRUE------------------------------------
#  plot(pl)
#  
#  node_rules(pl)
#  
#  top_items(pl, top = 5)
#  
#  worst_regret(pl)
#  
#  worth_map(pl)

