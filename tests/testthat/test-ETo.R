context("test-ETo")

library("gosset")
# load("tests/test_data.rda")
load("../test_data.rda")


e <- c(4.752054,4.897989,4.659525,3.947859,4.535946,
       4.231035,4.535325,4.390011,4.412367,4.124223)


test_that("equal", {
  
  ev <- suppressWarnings(
    ETo(temp,
        day.one = d, 
        span = 10,
        lat = rep(0, 10))
  )
    
  
  ev <- all.equal(e, ev[[1]])
  
  expect_equal(ev, TRUE)
})

test_that("error", {
  expect_error(
    ETo(temp,
        day.one = c(1:10), 
        span = 10,
        lat = rep(0, 10))
    )
})


test_that("nasapower works", {
  r <- suppressWarnings(
    ETo(object = lonlat, 
             day.one = d,
             span = 20,
             lat = lonlat[,2])
  )
  
  r <- !is.null(r)
  
  expect_equal(r, TRUE)
})


test_that("daytime hours", {
  
  r <- ETo(temp,
           day.one = d, 
           span = 10)
  
  r <- is.numeric(r[[1]])
  
  expect_equal(r, TRUE)
})


test_that("accept a tibble", {
  
  coord <- as.data.frame(lonlat)
  coord <- tibble::as_tibble(coord)
  
  e <- ETo(temp,
           day.one = d,
           span = 10,
           lat = coord[,2])
  
  e <- !any(is.na(e))
  
  expect_equal(e, TRUE)
  
  
})





