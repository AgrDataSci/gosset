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

test_that("pltree works", {

  df <- data.frame(x = rep(1, 6), y = rep(1, 6))
  
  df <- cbind(G, df)
  
  mod <- pltree(G ~ ., data = df)

  p <- pseudoR2(mod, newdata = df)
  
  expect_equal(is.data.frame(p), TRUE)
  
})


mod <- glm(Temp ~ Wind + Solar.R,
           data = airquality,
           family = poisson())

test_that("default works", {
  
  p <- pseudoR2(mod, newdata = airquality)
  
  expect_equal(is.data.frame(p), TRUE)
  
})



test_that("bttree works", {
  
  library("psychotree")
  
  pc <- rank_paircomp(G)
  
  df <- data.frame(x = rep(1, 6), y = rep(1, 6))
  
  df$pc <- pc
  
  mod <- bttree(pc ~ ., data = df)
  
  p <- pseudoR2(mod, newdata = df)
  
  expect_equal(is.data.frame(p), TRUE)
  
})
