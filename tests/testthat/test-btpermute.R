context("test-btpermute")

library("BradleyTerry2")

data("kenyachoice", package = "gosset")

selected <- c("LabourAvailability","InputChanges")

test_that("btpermute select", {
  
  mod <- suppressWarnings(
    btpermute(contests = kenyachoice$contests,
              predictors = kenyachoice$predictors,
              n.iterations = 10,
              seed = 1)
    )

  true <- all(mod$selected == selected)
  
  expect_true(true)
  
})
