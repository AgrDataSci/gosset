library("PlackettLuce")
library("gosset")
library('DescTools')
R = matrix(c(1, 2, 4, 3,
             1, 4, 2, 3,
             1, 2, NA, 3,
             1, 2, 4, 3,
             1, 3, 4, 2,
             1, 4, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) = LETTERS[1:4]

G = group(as.rankings(R), 1:6)

mod = pltree(G ~ 1, data = G)

preds = predict(mod, type = "rank")

kendallTau(R, preds)

tau = k[1,1]
n = k[1,2]

z = (3 * tau * sqrt(n*(n-1))) / sqrt(2 * (2*n + 5))

z

pnorm(z, lower.tail = F)


KendallTauB(x = R[1, ], y = preds[1, ], conf.level = 0.95)


