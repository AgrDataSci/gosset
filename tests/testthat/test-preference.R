context("test-preference")

library("gosset")
# load("tests/test_data.rda")
load("../test_data.rda")

test_that("accepts data = null", {
  
  pref <- preference(items = triadic[,c(1:3)],
                     input = triadic[,c(4:5)])
  
  expect_equal(is.data.frame(pref), TRUE)

})

test_that("accepts indexed items and input", {
  
  pref <- preference(data = triadic,
                     items = c(1:3),
                     input = c(4:5),
                     reorder = FALSE)
  
  
  expect_equal(is.data.frame(pref), TRUE)
  
})


test_that("accepts named items and input", {
  
  pref <- preference(data = triadic,
                    items = c("item_A","item_B","item_C"),
                    input = c("best","worst"))
  
  
  expect_equal(is.data.frame(pref), TRUE)
  
})


test_that("accepts 4 or more comparisons", {
  
  pref <- preference(data = tetra,
                   items = c(1:5),
                   input = c(6:10))
  
  expect_equal(is.data.frame(pref), TRUE)
  
})



test_that("accept additional rank", {
  
  library("PlackettLuce")
  data("beans", package = "PlackettLuce")

  pref <- preference(data = beans,
                     items = c(1:3),
                     input = c(4:5),
                     additional.rank = beans[c(6:8)])
  
  expect_equal(is.data.frame(pref), TRUE)
  
})


test_that("plot works", {
  
  pref <- preference(data = tetra,
                     items = c(1:5),
                     input = c(6:10))
  
  pref <- plot(pref)
  
  pref <- "ggplot" %in% class(pref)
  
  expect_equal(pref, TRUE)
  
})