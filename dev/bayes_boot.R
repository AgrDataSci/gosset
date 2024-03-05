# bayes bootstrapping 
library("PlackettLuce")
library("bayesboot")
library("gosset")

data("breadwheat", package = "gosset")

G = rank_tricot(breadwheat,
                items = c("variety_a","variety_b","variety_c"),
                input = c("overall_best","overall_worst"),
                group = FALSE)

# split the data into k-folds to run the models
# in a cross-validation frame work 
# with 5 folds, 20% of the data is removed in each 
# iteration, samples don't have replacement
# number of k will mean the number of original 
# values for the bayes bootstrapping 
n = nrow(G)
k = 5
set.seed(1044)
folds = sample(rep(1:k, times = ceiling(n/k), length.out = n))
table(folds)
sum(folds != 1)
estimates = data.frame()

# fit the model and get coefficients using itempar and qvcalc
for(i in seq_len(k)) {
  m = PlackettLuce(G[folds != i, ])
  m = qvcalc(itempar(m))$qvframe
  m$item = row.names(m)
  estimates = rbind(estimates, m)
}

boxplot(estimates$estimate ~ estimates$item)

# split the data frame by item name and run the boostrapping
bayes = split(estimates, estimates$item)

bayes = lapply(bayes, function(x){
  z = bayesboot(x$estimate, mean, R = 500)
  z$item = x$item
  z
})

bayes = do.call(rbind, bayes)

rownames(bayes) = 1:nrow(bayes)
names(bayes)[1] = "estimate"

boxplot(bayes$estimate ~ bayes$item)


