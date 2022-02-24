context("test-btpermute")

library("gnm")
library("foreach")
library("abind")
library("doParallel")

data("airquality")

selected <- c("Ozone", "Solar.R", "Month")

test_that("forward select airquality", {
  
  mod <- forward(Temp ~ .,
                 data = airquality,
                 k = 3,
                 select.by = "AIC",
                 packages = "gnm",
                 seed = 222,
                 family = poisson(link = "log"))
  
  vars <- all.vars(as.formula(mod$raw$call))[-1]
  
  true <- all(vars == selected)
  
  expect_true(true)
  
})


selected2 <- "Ozone"

test_that("forward select Akaike", {
  
  mod2 <- forward(Temp ~ .,
                 data = airquality,
                 k = 3,
                 packages = "gnm",
                 akaike.weights = TRUE,
                 seed = 222,
                 family = poisson(link = "log"))
  
  vars2 <- all.vars(as.formula(mod2$raw$call))[-1]
  
  true <- all(vars2 == selected2)
  
  expect_true(true)
  
})



# error

test_that("error invalid method", {
  
  expect_error(
    forward(Temp ~ .,
            data = airquality,
            k = 3,
            select.by = "aic")
    )
  
})