context("test-vitories")

library("gosset")
# load("tests/test_data.rda")
load("../test_data.rda")

data("breadwheat", package = "gosset")

R <- rank_tricot(data = breadwheat,
                 items = c(1:3),
                 input = c(18:19))

test_that("return a data.frame", {
  
  pref <- victories(R)
  
  pref <- dim(pref)
  
  pref <- any(pref != c(240,5)) 
  
  expect_equal(pref, FALSE)
  
})


test_that("plot works", {
  
  pref <- victories(R)
  
  pref <- plot(pref)
  
  pref <- "ggplot" %in% class(pref)
  
  expect_equal(pref, TRUE)
  
})