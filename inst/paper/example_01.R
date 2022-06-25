library("gosset")
library("PlackettLuce")
library("climatrends")
library("nasapower")
library("ggplot2")
library("chirps")

data("nicabean", package = "gosset")

dat <- nicabean$trial

covar <- nicabean$covar

traits <- unique(dat$trait)

R <- vector(mode = "list", length = length(traits))

for (i in seq_along(traits)) {
  
  dat_i <- subset(dat, dat$trait == traits[i])
  
  R[[i]] <- rank_numeric(data = dat_i,
                         items = "item",
                         input = "rank", 
                         id = "id", 
                         ascending = TRUE)
}

baseline <- which(grepl("OverallAppreciation", traits))

kendall <- lapply(R[-baseline], function(X){
  kendallTau(x = X, y = R[[baseline]])
})

kendall <- do.call("rbind", kendall)

kendall$trait <- traits[-baseline]

mod <- lapply(R, PlackettLuce)

worth_map(mod[-baseline],
          labels = traits[-baseline], 
          ref = "Amadeus 77") +
  labs(x = "Variety",
       y = "Trait")


dates <- c(min(covar[, "planting_date"]),
           max(covar[, "planting_date"]) + 70)

chirps <- get_chirps(covar[, c("longitude","latitude")], 
                     dates = as.character(dates),
                     as.matrix = TRUE,
                     server = "ClimateSERV")

# rename the matrix
newnames <- dimnames(chirps)[[2]]
newnames <- gsub("chirps-v2.0.", "", newnames)
newnames <- gsub("[.]", "-", newnames)

dimnames(chirps)[[2]] <- newnames

rain <- rainfall(chirps, day.one = covar$planting_date, span = 45)

Y <- which(grepl("Yield", traits))

Y <- group(R[[Y]], index = 1:length(R[[Y]]))

pldY <- cbind(Y, rain)

treeY <- pltree(Y ~ Rtotal, data = pldY, alpha = 0.1)

plot(treeY)

reliability(treeY, ref = "Amadeus 77")






