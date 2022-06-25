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

# chirps <- get_chirps(covar[, c("longitude","latitude")], 
#                      dates = as.character(dates),
#                      as.matrix = TRUE,
#                      server = "ClimateSERV")

load("inst/paper/nicabean_chirps.rda")

# rename the matrix
newnames <- dimnames(chirps)[[2]]
newnames <- gsub("chirps-v2.0.", "", newnames)
newnames <- gsub("[.]", "-", newnames)

dimnames(chirps)[[2]] <- newnames

rain <- rainfall(chirps, day.one = covar$planting_date, span = 45)

G <- which(grepl("Yield", traits))

G <- group(R[[G]], index = 1:length(R[[G]]))

pldG <- cbind(G, rain)

tree <- pltree(G ~ SDII, data = pldG, alpha = 0.1)

plot(tree)

reliability(tree, ref = "Amadeus 77")





