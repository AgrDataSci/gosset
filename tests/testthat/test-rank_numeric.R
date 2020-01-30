context("test-rank_numeric")

library("PlackettLuce")
# load("tests/test_data.rda")
load("../test_data.rda")

set.seed(123)
dt <- data.frame(id = rep(1:3, each = 4),
                 items = rep(LETTERS[1:4], times = 3),
                 input = runif(12, 1, 3),
  stringsAsFactors = FALSE)

dt <- dt[-c(1,6,9), ]

des_dt <- matrix(c(0, 2, 3, 1,
                   1, 0, 3, 2,
                   0, 2, 1, 3), 
                 nrow = 3, ncol = 4, byrow = TRUE)
dimnames(des_dt)[[2]] <- LETTERS[1:4]

test_that("rankings with floating numbers, higher better", {
  
  R <- rank_numeric(data = dt,
                    items = 2,
                    input = 3,
                    id = 1)
  
  R <- R[1:length(R),, as.rankings = FALSE]
  
  R <- all(R == des_dt)
  
  expect_true(R)
  
})

# here we test for ascending rankings 
# lower values are better
asc_dt <- matrix(c(0, 2, 1, 3,
                   3, 0, 1, 2,
                   0, 2, 3, 1), 
                 nrow = 3, ncol = 4, byrow = TRUE)
dimnames(asc_dt)[[2]] <- LETTERS[1:4]


test_that("rankings with floating numbers, lower better", {
  
  R <- rank_numeric(data = dt,
                    items = "items",
                    input = "input",
                    id = "id",
                    ascending = TRUE,
                    group = TRUE)
  
  R <- R[1:length(R), , as.grouped_rankings = FALSE]
  
  R <- all(R == asc_dt)
  
  expect_true(R)
  
})


# now we test it with integers
dt <- data.frame(id = rep(1:3, each = 4),
                 items = rep(LETTERS[1:4], times = 3),
                 input = c(1,3,2,4,
                           3,4,2,1,
                           1,2,3,4),
stringsAsFactors = FALSE)

dt <- dt[-c(1,6), ]

dt

# first for ascending, this means that lower are better 
dt_r <- matrix(c(0, 2, 1, 3,
                 3, 0, 2, 1,
                 1, 2, 3, 4), 
               nrow = 3, ncol = 4, byrow = TRUE)

dimnames(asc_dt)[[2]] <- LETTERS[1:4]

test_that("rankings with integers, lower better", {
  
  R <- rank_numeric(data = dt,
                    items = "items",
                    input = "input",
                    id = "id")
  
  R <- R[1:length(R), , as.grouped_rankings = FALSE]
  
  R <- all(R == dt_r)
  
  expect_true(R)
  
})


test_that("argument ascending does not affect integers", {
  
  R <- rank_numeric(data = dt,
                    items = "items",
                    input = "input",
                    id = "id",
                    ascending = TRUE)
  
  R <- R[1:length(R), , as.rankings = FALSE]
  
  R <- all(R == dt_r)
  
  expect_true(R)
  
})


# now test for rankings provided in the wide format
dt <- do.call("cbind", default_wide)
dt <- dt[1:3,]
dt[1,6] <- NA

dt_w <- matrix(c(2,3,0,1,4,
                 3,4,1,5,2,
                 5,1,2,3,4), 
               nrow = 3, ncol = 5, byrow = TRUE)


test_that("wide format gives right rankings", {
  R <- rank_numeric(dt,
                    items = paste0("Item",1:5),
                    input = paste0("Position_Item",1:5))
  
  
  R <- R[1:length(R), , as.rankings = FALSE]
  
  R <- all(R == dt_w)
  
  expect_true(R)
  
})


# test errors
test_that("null data gives an error", {
  
  expect_error(rank_numeric(items = default_wide[[1]],
                            input = default_wide[[2]],
                            group = TRUE))
  
})




