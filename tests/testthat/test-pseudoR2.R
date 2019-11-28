context("test-pseudoR2")


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

test_that("pltree works", {
  
  p <- pseudoR2(mod)
  
  expect_equal(is.data.frame(p), TRUE)
  
})


mod <- glm(Temp ~ Wind + Solar.R,
           data = airquality,
           family = poisson())

pseudoR2(mod)

test_that("default works", {
  
  p <- pseudoR2(mod)
  
  expect_equal(is.data.frame(p), TRUE)
  
})

library("psychotree")

test_that("bttree works", {
  
  pc <- rank_paircomp(G)
  
  df <- data.frame(x = rep(1, 6), y = rep(1, 6))
  
  df$pc <- pc
  
  mod <- bttree(pc ~ ., data = df)
  
  p <- pseudoR2(mod)
  
  expect_equal(is.data.frame(p), TRUE)
  
})
