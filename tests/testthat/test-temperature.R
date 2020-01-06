context("test-temperature")

library("gosset")
# load("tests/test_data.rda")
load("../test_data.rda")


maxDT <- c(33.5,34.9,32,27.5,32.8,27.4,32.6,31.6,32.3,28.5)
minNT <- c(8.2,9.1,8.2,1.5,5.7,6,5.7,4.6,4.3,2.8)

test_that("day equal", {
  skip_on_cran()
  x <- temperature(object = temp, 
                   day.one = d,
                   span = 8)
  
  dt <- all.equal(maxDT, x$maxDT)
  
  expect_equal(dt, TRUE)
})

test_that("night equal", {
  skip_on_cran()
  x <- temperature(object = temp, 
                   day.one = d,
                   span = 8)
  
  nt <- all.equal(minNT, x$minNT)
  
  expect_equal(nt, TRUE)
})

test_that("nasapower works", {
  skip_on_cran()
  r <- suppressWarnings(
    temperature(object = lonlat, 
                   day.one = d,
                   span = 25,
                   index = "maxDT")
  )
  
  r <- as.vector(apply(r, 1, is.na))
  
  r <- sum(r) == 0
  
  expect_equal(r, TRUE)
})



