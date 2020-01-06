context("test-GDD")

library("gosset")
# load("tests/test_data.rda")
load("../test_data.rda")

g <- c(5,4,5,11,5,7,5,6,6,8)

test_that("equal", {
  skip_on_cran()
  dg <- GDD(object = temp,
            day.one = d, 
            degree.days = 45,
            base = 10, 
            span = 12)
  
  
  dg <- all.equal(g, dg[[1]])
  
  expect_equal(dg, TRUE)
})


test_that("nasapower works", {
  skip_on_cran()
  dg <- suppressWarnings(
    GDD(object = lonlat,
        day.one = d, 
        degree.days = 45, 
        span = 12)
  )
  
  dg <- as.vector(apply(dg, 1, is.na))
  
  dg <- sum(dg) == 0
  
  expect_equal(dg, TRUE)
})


test_that("missing day.one", {
  skip_on_cran()
  expect_error(
    GDD(object = temp)
  )
})


test_that("missing degree.days", {
  skip_on_cran()
  expect_error(
    GDD(object = temp, 
        day.one = d)
  )
})


test_that("missing span", {
  skip_on_cran()
  expect_error(
    GDD(object = temp, 
        day.one = d, 
        degree.days = 10)
  )
})