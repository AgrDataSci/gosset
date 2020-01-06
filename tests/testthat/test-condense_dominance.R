context("test-dominance")

data("breadwheat", package = "gosset")

R <- rank_tricot(data = breadwheat,
                 items = c(1:3),
                 input = c(18:19))

test_that("return a data.frame", {
  
  pref <- recap_dominance(R)
  
  pref <- dim(pref)
  
  pref <- any(pref != c(240, 4)) 
  
  expect_equal(pref, FALSE)
  
})


test_that("plot works", {
  
  pref <- recap_dominance(R)
  
  pref <- plot(pref)
  
  pref <- "ggplot" %in% class(pref)
  
  expect_equal(pref, TRUE)
  
})