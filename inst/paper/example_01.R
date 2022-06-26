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

pldG$Rtotal <- round(pldG$Rtotal, 2)

tree <- pltree(G ~ Rtotal, data = pldG, alpha = 0.1)

plot(tree, ref = "Amadeus 77", ci.level = 0.9)
node_labels(tree)
node_rules(tree)
top_items(tree, top = 3)

ggsave("inst/paper/pltree_01.png", 
       last_plot(),
       width = 10,
       height = 10)


reliability(tree, ref = "Amadeus 77")


rel <- reliability(tree, ref = "Amadeus 77")

rel <- rel[rel$reliability >= 0.5, ]

rel <- rel[c(1:5)]

rel[c(3:5)] <- lapply(rel[c(3:5)], function(x){round(x, 3)})

rel
