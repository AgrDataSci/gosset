test_that("single comparison matches stats::cor", {
  x = c(1, 2, 3, 4)
  y = c(1, 3, 2, 4)
  kt = kendallTau(x, y)
  expect_equal(kt$kendallTau, stats::cor(x, y, method = "kendall"))
  expect_equal(kt$N_effective, 4)
})

test_that("rows are weighted by the number of pairs they compare", {
  # one row with four ranked items (6 pairs) and one with two (1 pair)
  x = rbind(c(1, 2, 3, 4), c(1, 2, 0, 0))
  y = rbind(c(1, 2, 4, 3), c(2, 1, 0, 0))
  kt = kendallTau(x, y)
  tau1 = stats::cor(x[1, ], y[1, ], method = "kendall")
  tau2 = -1
  expected = (tau1 * 6 + tau2 * 1) / 7
  expect_equal(kt$kendallTau, expected)
  # effective N recovers the total of 7 pairs
  n = kt$N_effective
  expect_equal(n * (n - 1) / 2, 7)
})

test_that("triads give the same result as before the weighting fix", {
  x = rbind(c(1, 2, 3), c(2, 1, 3), c(3, 1, 2))
  y = rbind(c(1, 3, 2), c(2, 1, 3), c(1, 2, 3))
  kt = kendallTau(x, y)
  taus = vapply(1:3, function(i) stats::cor(x[i, ], y[i, ], method = "kendall"), numeric(1))
  expect_equal(kt$kendallTau, mean(taus))
})
