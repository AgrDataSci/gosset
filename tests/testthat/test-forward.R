context("test-forward")

library("gosset")
library("doParallel")
library("gnm")

data("airquality")

k <- 3
folds <- rep(1:3, times = nrow(airquality)/3)

mod1 <- forward(Temp ~ .,
                data = airquality, 
                k = k,
                folds = folds,
                select.by = "AIC",
                family = poisson(link = "log"))


mod2 <- forward(Temp ~ .,
                data = airquality, 
                k = k,
                folds = folds,
                select.by = "deviance",
                akaike.weights = TRUE,
                family = poisson(link = "log"))

mod1 <- as.vector(t(mod1$coeffs))
coeffs1 <-
  c(252.9602385,
    244.9602385,
    -122.4801192,
    0.4125532,
    0.4145210,
    0.1001847)


mod2 <- as.vector(t(mod2$coeffs))
coeffs2 <-
  c(265.9658315,
    261.9658315,
    -130.9829158,
    0.3515344,
    0.3528859,
    0.0777664)

test_that("model with deviance", {
  expect_equal(mod1, coeffs1)
})

test_that("model with deviance mean equal", {
  expect_equal(mod2, coeffs2)
})