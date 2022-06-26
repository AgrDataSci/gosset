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

yield <- which(grepl("Yield", traits))

G <- group(R[[yield]], index = 1:length(R[[yield]]))

pldG <- cbind(G, rain)

pldG$Rtotal <- round(pldG$Rtotal, 2)

tree <- pltree(G ~ Rtotal, data = pldG, alpha = 0.1)

plot(tree, ref = "Amadeus 77", ci.level = 0.9)
node_labels(tree)
node_rules(tree)
top_items(tree, top = 3)

ggsave("inst/paper/pltree_01.png", 
       last_plot(),
       width = 25,
       height = 20,
       dpi = 300,
       units = "cm")


reliability(tree, ref = "Amadeus 77")


rel <- reliability(tree, ref = "Amadeus 77")

rel <- rel[rel$reliability >= 0.5, ]

rel <- rel[c(1:5)]

rel[c(3:5)] <- lapply(rel[c(3:5)], function(x){round(x, 3)})

rel

regret(tree)

Overall <- PlackettLuce(R[[baseline]])
Yield <- PlackettLuce(R[[yield]])

compare(Overall, Yield) +
  labs(x = "Average log(worth)",
       y = "Difference (Overall Appreciation - Yield)")


comp <- compare(Overall, Yield) +
  labs(x = "Average log(worth)",
       y = "Difference (Overall Appreciation - Yield)")


ggsave("inst/paper/compare_02.png", 
      comp,
      width = 15,
      height = 15,
      dpi = 300,
      units = "cm")
