context("test-rainfall")

library("gosset")
# load("tests/test_data.rda")
load("../test_data.rda")

MLDS <- c(rep(9, 2), rep(10, 8))
MLWS <- c(rep(1, 2), rep(0, 8))

test_that("dry equal", {
  skip_on_cran()
  r <- rainfall(object = rain, 
                day.one = d,
                span = 10)
  
  ds <- all.equal(MLDS, r$MLDS)
  
  expect_equal(ds, TRUE)
})

test_that("moist equal", {
  skip_on_cran()
  r <- rainfall(object = rain, 
                day.one = d,
                span = 10)
  
  ws <- all.equal(MLWS, r$MLWS)
  
  expect_equal(ws, TRUE)
})

test_that("nasapower works", {
  skip_on_cran()
  r <- suppressWarnings(
    rainfall(object = lonlat, 
                day.one = d,
                span = 25)
  )
  
  r <- as.vector(apply(r, 1, is.na))
  
  r <- sum(r) == 0
  
  expect_equal(r, TRUE)
})


test_that("no day.one", {
  expect_error(
    rainfall(object = rain,
             day.one = NULL,
             span = 10)
  )
})

test_that("no span", {
  expect_error(
    rainfall(object = rain,
             day.one = d,
             span = NULL)
  )
})
