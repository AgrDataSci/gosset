context("test-kendallTau")


library("PlackettLuce")

R <- matrix(c(1, 2, 3, 4,
              1, 2, 3, 4,
              1, 2, 3, 4,
              1, 2, 3, 4,
              1, 2, 3, 4,
              1, 2, 3, 4), nrow = 6, byrow = TRUE)
colnames(R) <- LETTERS[1:4]

G <- group(as.rankings(R), 1:6)

mod <- pltree(G ~ 1, data = G)

preds <- predict(mod)



test_that("kendall works", {
  
  k <- kendallTau(R, preds)
  
  k <- !any(as.vector(k) == c(1,9))
  
  expect_equal(k, FALSE)
  
})
