context("test-rank_tricot")

library("gosset")
library("PlackettLuce")
# load("tests/test_data.rda")
load("../test_data.rda")

# a simple triadic comparison
# three items ranked as best and worst 
# with A, B or C
test_that("triadic", {
  
  R <- rank_tricot(data = triadic,
                   items = c(1:3),
                   input = c(4:5), 
                   group = TRUE)
  
  R <- R[1:length(R),, as.grouped_rankings = FALSE]
  
  expect_true(all(R == triadic_true))
  
})

# # add comparison with local item into the tricot 
# # comparison is labelled as Better or Worse 
# test_that("triadic with local", {
#   
#   R <- rank_tricot(data = triadic,
#                    items = c(1:3),
#                    input = c(4:5),
#                    additional.rank = triadic[, 6:8],
#                    group = TRUE)
#   
#   R <- R[1:length(R),, as.grouped_rankings = FALSE]
#   
#   R <- all(R == triadic_true_local)
#   
#   expect_true(R, TRUE)
#   
# })

# four or more comparison in a tricot project
test_that("tretra ok", {
  
  R <- rank_tricot(data = tetra,
                   items = c(1:5),
                   input = c(6:10),
                   group = TRUE)
  
  R <- R[1:length(R),, as.grouped_rankings = FALSE]
  
  expect_true(all(R == tetra_true))
  
})

# test if full.output works
# it returns a list with a rankings, grouped_rankings 
# and a decoded order for items A B and C
test_that("full.output works", {
  
  R <- rank_tricot(data = triadic,
                   items = c(1:3),
                   input = c(4:5),
                   additional.rank = triadic[,6:8],
                   full.output = TRUE)
  
  l <- c(!is.list(R), unlist(lapply(R, is.null)))

  expect_equal(any(l), FALSE)
  
})

# pseudo item is a way to keep ranking when at least
# the information for two items is available
test_that("pseudo item tricot", {
  
  p <- triadic
  p[1, 1] <- NA
  
  p <- rank_tricot(data = p,
                   items = c(1:3),
                   input = c(4:5), 
                   group = TRUE)
  
  p <- p[1:length(p) , , as.grouped_rankings = FALSE]
  
  p <- sum(rowSums(p) == c(3, 6, 6, 6, 6))
  
  expect_equal(p, 5)
  
})

test_that("pseudo item tetra", {
  
  p <- tetra
  p[1, 1] <- NA
  p[1:2, 6] <- NA
  
  p <- rank_tricot(data = p,
                   items = c(1:5),
                   input = c(6:10))
  
  pv <- matrix(c(4,0,3,2,1,
                 0,2,1,3,4,
                 3,2,1,5,4,
                 2,1,5,3,4,
                 2,5,4,1,3), nrow = 5, ncol = 5, byrow = TRUE)
  
  p <- p[1:length(p) , , as.rankings = FALSE]
  
  expect_true(all(p == pv))
  
})

