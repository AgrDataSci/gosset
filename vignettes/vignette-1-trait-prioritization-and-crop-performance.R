## ----starting, message = FALSE, eval = TRUE, echo = TRUE----------------------
library("gosset")
library("PlackettLuce")
library("climatrends")
library("chirps")
library("ggplot2")

data("nicabean", package = "gosset")

dat = nicabean$trial

covar = nicabean$covar

traits = unique(dat$trait)

dat


## ----rankings, message = FALSE, eval = TRUE, echo = TRUE----------------------
R = vector(mode = "list", length = length(traits))

for (i in seq_along(traits)) {

  dat_i = subset(dat, dat$trait == traits[i])

  R[[i]] = rank_numeric(data = dat_i,
                         items = "item",
                         input = "rank",
                         id = "id",
                         ascending = TRUE)
}


## ----kendall1, message=FALSE, eval=TRUE, echo=TRUE----------------------------
baseline = which(grepl("OverallAppreciation", traits))

kendall = lapply(R[-baseline], function(X){
  kendallTau(x = X, y = R[[baseline]])
})

kendall = do.call("rbind", kendall)

kendall$trait = traits[-baseline]

## ----kendall2, message=FALSE, eval=TRUE, echo=FALSE---------------------------

kendall = kendall[,c(5, 1:4)]
kendall[,2:4] = lapply(kendall[,2:4], function(x) round(x, 3))

kendall[,5] = formatC(kendall[,5], format = "e")

kendall = kendall[order(kendall$kendallTau), ]

kendall


## ----kendall_boost, message = FALSE, eval = TRUE, echo = TRUE-----------------

# lapply the bootstrap function and draw 50 data points
kendall = lapply(R[-baseline], function(x){
  kendallTau_bootstrap(x, 
                       R[[baseline]],
                       nboot = 50,
                       seed = 1206)
})

# put it in a data.frame
kendall = data.frame(trait = rep(traits[-baseline], each = 50),
                     kendallTau = unlist(kendall))

# define levels of traits to sort them out from highest to lowest kendall tau
lvls = unique(kendall$trait[order(kendall$kendallTau)])

kendall$trait = factor(kendall$trait, levels = lvls)

# plot the coefficients 
ggplot(kendall, aes(y = trait, x = kendallTau)) +
  geom_boxplot() +
  labs(y = "", x = "Correlation with the 'Overall appreciation'") +
  theme_minimal()


## ----PLmodel, message=FALSE, eval=TRUE, echo=TRUE-----------------------------

mod = lapply(R, PlackettLuce)


## ----worthmap, message=FALSE, eval=TRUE, echo=TRUE----------------------------
worth_map(mod,
          labels = traits,
          labels.order = rev(traits)) +
  labs(x = "Variety",
       y = "Trait")

## ----chirps, message=FALSE, eval=FALSE, echo=TRUE-----------------------------
#  dates = c(min(covar[, "planting_date"]),
#             max(covar[, "planting_date"]) + 70)
#  
#  chirps = get_chirps(covar[, c("longitude","latitude")],
#                       dates = as.character(dates),
#                       as.matrix = TRUE,
#                       server = "ClimateSERV")

## ----chirps2, message=FALSE, eval=TRUE, echo=FALSE----------------------------

load("nicabean_chirps.rda")


## ----chirps3, message=FALSE, eval=TRUE, echo=TRUE-----------------------------
newnames = dimnames(chirps)[[2]]
newnames = gsub("chirps-v2.0.", "", newnames)
newnames = gsub("[.]", "-", newnames)

dimnames(chirps)[[2]] = newnames

rain = rainfall(chirps, day.one = covar$planting_date, span = 45)

## ----grouped_ranking, message=FALSE, eval=TRUE, echo=TRUE---------------------
yield = which(grepl("Yield", traits))

G = group(R[[yield]], index = 1:length(R[[yield]]))

head(G)


## ----pltree, message=FALSE, eval=TRUE, echo=TRUE------------------------------
pldG = cbind(G, rain)

tree = pltree(G ~ Rtotal, data = pldG, alpha = 0.1)

print(tree)


## ----node_info, message=FALSE, eval=TRUE, echo=TRUE---------------------------
node_labels(tree)

node_rules(tree)

top_items(tree, top = 3)

## ----node_info2, message=FALSE, eval=TRUE,echo=TRUE---------------------------
plot(tree, ref = "Amadeus 77")

## ----rel1, message=FALSE, eval=FALSE, echo=TRUE-------------------------------
#  reliability(tree, ref = "Amadeus 77")

## ----rel2, message=FALSE, eval=TRUE, echo=FALSE-------------------------------

rel = reliability(tree, ref = "Amadeus 77")

rel = rel[rel$reliability >= 0.5, ]

rel = rel[c(1:5)]

rel


## ----compare, message=FALSE, echo=TRUE, eval=TRUE, out.width="50%"------------
Overall = PlackettLuce(R[[baseline]])
Yield = PlackettLuce(R[[yield]])

compare(Overall, Yield) +
  labs(x = "Average log(worth)",
       y = "Difference (Overall appreciation - Yield)")

