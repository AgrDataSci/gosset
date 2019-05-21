context("test-forward")

library("gosset")
library("psychotree")
library("doParallel")

data("Topmodel2007", package = "psychotree")

head(Topmodel2007)

data <- Topmodel2007

folds <- ifelse(data$age < 30, 1, 
                ifelse(data$age >= 30 & data$age < 55, 2, 3))

k <- max(folds)

mod1 <- forward(preference ~ .,
                data = Topmodel2007,
                k = k,
                folds = folds,
                akaike.weights = TRUE,
                select.by = "deviance")

mod2 <- forward(preference ~ .,
                data = Topmodel2007,
                k = k,
                folds = folds,
                select.by = "deviance",
                mean.method = "equal")

mod1 <- as.vector(t(mod1$coeffs))
coeffs1 <- c(1402.62558035,
             1383.25579622,
             -691.62789811,
             0.09378074,
             0.12504098,
             0.07111604)


mod2 <- as.vector(t(mod2$coeffs))
coeffs2 <- c(1257.32514417,
             1239.32514417,
             -619.66257209,
             0.09761283,
             0.13015044,
             0.07419328)

test_that("model with deviance", {
  expect_equal(mod1, coeffs1)
})

test_that("model with deviance mean equal", {
  expect_equal(mod2, coeffs2)
})