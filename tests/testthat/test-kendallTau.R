context("test-kendallTau")
library("PlackettLuce")
library("gosset")

M <- matrix(c(1, 2, 3, 4,
              1, 2, 3, 4,
              1, 2, 3, 4,
              1, 2, 3, 4,
              1, 2, 3, 4,
              1, 2, 3, 4), nrow = 6, byrow = TRUE)
colnames(M) <- LETTERS[1:4]

R <- as.rankings(M)

G <- group(R, 1:6)

mod <- pltree(G ~ 1, data = G)

preds <- predict(mod)

test_that("kendall vector", {
  
  x <- M[1,]
  
  y <- predict(mod)[1,]
  
  k <- kendallTau(x, y)
  
  k <- k$kendallTau == 1 
    
  expect_true(k)
  
})


test_that("kendall matrix", {
  
  Y <- predict(mod)
  
  k <- kendallTau(Y, Y)
  
  k <- k$kendallTau == 1 
  
  expect_true(k)
  
})


# null.rm FALSE
test_that("kendall data.frame", {
  
  Y <- as.data.frame(predict(mod))
  
  Y[1,2] <- 0
  Y[3,4] <- 0
  
  X <- as.data.frame(M)
  
  k <- kendallTau(X, Y, null.rm = FALSE)
  
  k <- k$kendallTau
  
  k <- round(k, 1)
  
  k <- k == 0.9
  
  expect_true(k)
  
})


test_that("kendall rankings", {
  
  k <- kendallTau(R, R, null.rm = FALSE)
  
  k <- k$kendallTau
  
  k <- k == 1
  
  expect_true(k)
  
})


test_that("kendall grouped_rankings", {
  
  k <- kendallTau(G, G, null.rm = FALSE)
  
  k <- k$kendallTau
  
  k <- k == 1
  
  expect_true(k)
  
})

