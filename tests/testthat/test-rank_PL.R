context("test-rank_PL")

library("gosset")
library("PlackettLuce")
# compare it with the rankings given by to_rankings
# load("tests/tricot.rda")
load("../tricot.rda")

testR <- beans[1:10, c(1:5)] 

testR$middle <- complete(testR[c("best", "worst")],
                         items = c("A", "B", "C"))

testR <- decode(testR[,c("best", "middle", "worst")],
                items = testR[,c(1:3)],
                code = LETTERS[1:3])

testR <- as.rankings(testR, input = "ordering")


testR <- testR[,, as.rankings = FALSE]


test_that("tricot ok", {
  
  toTest <- rank_PL(beans[1:10,],
                    items = c("variety_a","variety_b","variety_c"),
                    input = c("best","worst"),
                    type = "tricot")
  
  toTest <- toTest[,, as.rankings = FALSE]
  
  
  expect_false(any(testR != toTest))
})
