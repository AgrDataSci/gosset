library("gosset")
library("ClimMobTools")
library("PlackettLuce")

tech = c("apple", "banana", "pear", "orange", "grape")

set.seed(1951)
R = randomise(6, tech)

R$best = c("C", "B", "A", "B", "Not observed", "A")
R$worst = c("A", "Tie", "C", "Not observed", "Not observed", "C")

R

r1 = rank_tricot(R, items = 1:3, input = 4:5)

r1 = unclass(r1)

d = data.frame()
for (i in seq_len(nrow(r1))) {
  x = r1[i, ]
  x = x[x!=0]
  x = data.frame(id = i,
                 tech = names(x),
                 rank = x)
  d = rbind(d, x)
}

rownames(d) = 1:nrow(d)

d

rank_tricot(R, items = 1:3, input = 4:5, validate.rankings = T)

r2
