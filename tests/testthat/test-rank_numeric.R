context("test-rank_numeric")

library("gosset")
library("PlackettLuce")
# load("tests/test_data.rda")
load("../test_data.rda")


test_that("default ok", {
  
  R <- rank_numeric(data = default,
                    items = 2,
                    input = 3,
                    id = 1)
  
  R <- R[1:length(R),, as.rankings = FALSE]
  
  expect_equal(!any(R == default_true), FALSE)
  
})


test_that("long and ascending ok", {
  
  R <- rank_numeric(data = default,
                    items = 2,
                    input = 3,
                    id = 1,
                    ascending = TRUE,
                    group = TRUE)
  
  R <- R[1:length(R),, as.grouped_rankings = FALSE]
  
  Ras <- t(apply(default_true, 1, function(x) rank(x)))
  
  expect_equal(!any(R == Ras), FALSE)
  
})

test_that("wide ok", {
  
  df <- do.call(cbind, default_wide)
  
  R <- rank_numeric(data = df,
                    items = c(1:5),
                    input = c(6:10),
                    group = TRUE)
  
  R <- R[1:length(R),, as.grouped_rankings = FALSE]
  
  expect_equal(!any(R == default_wide_true), FALSE)
  
})


test_that("null data", {
  
  expect_error(rank_numeric(items = default_wide[[1]],
                             input = default_wide[[2]],
                             group = TRUE))
  
})

# # 
# save(d, lonlat, rain, tetra, tetra_true, triadic,
#      triadic_true, triadic_true_local, default,
#      default_true, temp, tricot, default_wide, default_wide_true,
#      file = "tests/test_data.rda")
