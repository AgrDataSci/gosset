context("test-coefficients")

library("gosset")
library("PlackettLuce")

load("../commonbeans.rda")

.mean_cv <- function(object, folds = NULL, 
                                  mean.method = NULL, 
                                  ...){
  # take length of folds
  N <- length(folds)
  
  foldsize <- as.vector(table(folds))
  
  mean <- sum(object * foldsize, na.rm = TRUE) / sum(foldsize, na.rm = TRUE)
  

}


# split data into lists with training and test set
train <- list()
for (i in 1:k) {
  train[[i]] <- df[folds != i ,]
}

test <- list()
for (i in 1:k) {
  test[[i]] <- df[folds == i  ,]
}

pl1 <- pltree(G ~ maxNT_GRA_acc,
              data = train[[1]],
              alpha = 0.01,
              minsize = 250,
              bonferroni = TRUE,
              npseudo = 1.5)

pl2 <- pltree(G ~ maxNT_GRA_acc,
              data = train[[2]],
              alpha = 0.01,
              minsize = 250,
              bonferroni = TRUE,
              npseudo = 1.5)

pl3 <- pltree(G ~ maxNT_GRA_acc,
              data = train[[3]],
              alpha = 0.01,
              minsize = 250,
              bonferroni = TRUE,
              npseudo = 1.5)

pl4 <- pltree(G ~ maxNT_GRA_acc,
              data = train[[4]],
              alpha = 0.01,
              minsize = 250,
              bonferroni = TRUE,
              npseudo = 1.5)

pl5 <- pltree(G ~ maxNT_GRA_acc,
              data = train[[5]],
              alpha = 0.01,
              minsize = 250,
              bonferroni = TRUE,
              npseudo = 1.5)

MaxLik <- rep(NA, 5)
MaxLik[1] <- pseudoR2(pl1)[[3]]
MaxLik[2] <- pseudoR2(pl2)[[3]]
MaxLik[3] <- pseudoR2(pl3)[[3]]
MaxLik[4] <- pseudoR2(pl4)[[3]]
MaxLik[5] <- pseudoR2(pl5)[[3]]

MaxLik <- .mean_cv(MaxLik, folds, "foldsize")

test_that("model with deviance", {
  expect_equal(MaxLik, coeffs[[3,5]])
})

