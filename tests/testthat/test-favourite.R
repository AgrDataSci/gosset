context("test-favourite")

library("gosset")
# load("tests/test_data.rda")
load("../test_data.rda")

test_that("accepts indexed items and input", {
  
  fav <- favourite(data = triadic,
                   items = c(1:3),
                   input = c(4:5),
                   reorder = FALSE)
  
  
  expect_equal(is.data.frame(fav), TRUE)
  
})


test_that("accepts named items and input", {
  
  fav <- favourite(data = triadic,
                   items = c("item_A","item_B","item_C"),
                   input = c("best","worst"))
  
  
  expect_equal(is.data.frame(fav), TRUE)
  
})


test_that("accepts 4 or more comparisons", {
  
  fav <- favourite(data = tetra,
                   items = c(1:5),
                   input = c(6:10))
  
  expect_equal(is.data.frame(fav), TRUE)
  
})



test_that("alias works", {
  
  fav <- favorite(data = tetra,
                  items = c(1:5),
                  input = c(6:10))
  
  expect_equal(is.data.frame(fav), TRUE)
  
})


test_that("plot works", {
  
  fav <- favorite(data = tetra,
                  items = c(1:5),
                  input = c(6:10))
  
  fav <- plot(fav)
  
  fav <- "ggplot" %in% class(fav)
  
  expect_equal(fav, TRUE)
  
})