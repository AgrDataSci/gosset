library(dominanceanalysis)
library(caTools)
library("gosset")
library("ClimMobTools")
library("PlackettLuce")
library("ggplot2")
library("patchwork")

data("cassava", package = "gosset")

dat = cassava

head(dat[, 1:11])

keep = unlist(lapply(dat[1:ncol(dat)], function(x) sum(is.na(x))))

keep = keep == 0

dat = dat[, keep]

names(dat)

# extract list of traits from the data
trait_list = getTraitList(dat, pattern = c("_pos", "_neg"))

# trait names extracted from the function 
traits = unlist(lapply(trait_list, function(x) x$trait_label))

# clean trait names and put them title case
traits = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", traits, perl = TRUE)

traits

pack = c("option_a", "option_b", "option_c")

items = sort(unique(unlist(dat[pack])))

check = "Obasanjo-2"

ov = which(traits %in% "Overall")

R = lapply(trait_list, function(x) {
  rank_tricot(dat, 
              items = pack, 
              input = x$string,
              validate.rankings = TRUE)
})

mod = lapply(R, PlackettLuce)

worth = lapply(mod, function(x){
  z = resample(x, log = TRUE, bootstrap = TRUE, n1 = 10, seed = 1432)$estimate
  #log(z)
})

worth = as.data.frame(do.call("cbind", worth))

names(worth) = traits

head(worth)

plot(worth$Colour, worth$Overall)
plot(worth$Stretchability, worth$Overall)
plot(worth$Taste, worth$Overall)

modpres = lm(Overall ~ Taste + Stretchability + Colour, data = worth)

summary(modpres)

dapres = dominanceAnalysis(modpres)

getFits(dapres,"r2")

dominanceMatrix(dapres, type="complete",fit.functions = "r2", ordered=TRUE)

contributionByLevel(dapres,fit.functions="r2")

plot(dapres, which.graph ="conditional",fit.function = "r2") + theme_minimal()

dom = averageContribution(dapres,fit.functions = "r2")


