context("test-worst_regret")


library("PlackettLuce")
data("breadwheat", package = "gosset")

# convert the tricot rankings from breadwheat data
# into a object of class 'grouped_rankings'

G <- rank_tricot(breadwheat,
                 items = c("variety_a","variety_b","variety_c"),
                 input = c("overall_best","overall_worst"),
                 group = TRUE)


# combine grouped rankings with temperature indices
mydata <- cbind(G, breadwheat[c("lon","lat")])

# fit a pltree model using geographic data
mod <- pltree(G ~ ., data = mydata)

test_that("worst_regret", {
  
  wr <- worst_regret(mod)
  
  expect_equal(is.data.frame(wr), TRUE)
  
})


test_that("winprobs works", {

  expect_error(worst_regret(mod, winprobs = rev(winprobs)))
  
})
