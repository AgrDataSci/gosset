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

test_that("p-values follow the chosen alternative", {
  x = c(1, 2, 3, 4, 5, 6)
  y = c(2, 1, 3, 5, 4, 6)
  two = kendallTau(x, y)
  z = two$Zvalue
  expect_equal(two[["Pr(>|z|)"]], 2 * stats::pnorm(-abs(z)))
  expect_equal(kendallTau(x, y, alternative = "greater")[["Pr(>|z|)"]],
               stats::pnorm(z, lower.tail = FALSE))
  expect_equal(kendallTau(x, y, alternative = "less")[["Pr(>|z|)"]],
               stats::pnorm(z))
})

test_that("matrix z uses the null variance of the weighted mean tau", {
  x = rbind(c(1, 2, 3, 4), c(1, 2, 0, 0), c(3, 1, 2, 0))
  y = rbind(c(1, 2, 4, 3), c(2, 1, 0, 0), c(3, 2, 1, 0))
  kt = kendallTau(x, y)
  n = c(4, 2, 3)
  pairs = n * (n - 1) / 2
  taus = vapply(1:3, function(i) {
    k = x[i, ] != 0 & y[i, ] != 0
    stats::cor(x[i, k], y[i, k], method = "kendall")
  }, numeric(1))
  w = pairs / sum(pairs)
  v = sum(w^2 * 2 * (2 * n + 5) / (9 * n * (n - 1)))
  expect_equal(kt$Zvalue, sum(w * taus) / sqrt(v))
})

test_that("z is calibrated under no association in many random triads", {
  skip_on_cran()
  set.seed(123)
  z = replicate(200, {
    x = t(replicate(150, sample(1:3)))
    y = t(replicate(150, sample(1:3)))
    kendallTau(x, y)$Zvalue
  })
  expect_gt(stats::sd(z), 0.85)
  expect_lt(stats::sd(z), 1.15)
  expect_lt(mean(abs(z) > 1.96), 0.10)
})

test_that("kendallTau_bootstrap resamples participants", {
  set.seed(21)
  n = 500
  x = t(replicate(n, sample(1:3)))
  y = t(replicate(n, sample(1:3)))
  
  b = kendallTau_bootstrap(x, y, nboot = 400, seed = 3)
  expect_length(b, 400)
  expect_true(all(b >= -1 & b <= 1))
  
  # centred on the average tau
  kt = kendallTau(x, y)
  expect_lt(abs(mean(b) - kt$kendallTau), 0.01)
  
  # spread close to the null standard error of the average tau
  # (variance of tau for one triad is 2 * 11 / (9 * 6))
  se = sqrt(2 * 11 / (9 * 3 * 2) / n)
  expect_gt(sd(b) / se, 0.8)
  expect_lt(sd(b) / se, 1.2)
})

test_that("kendallTau_bootstrap is reproducible and keeps the caller's RNG", {
  x = t(replicate(50, sample(1:3)))
  y = t(replicate(50, sample(1:3)))
  set.seed(5)
  before = runif(1)
  set.seed(5)
  b1 = kendallTau_bootstrap(x, y, nboot = 20, seed = 9)
  after = runif(1)
  expect_equal(before, after)
  expect_equal(b1, kendallTau_bootstrap(x, y, nboot = 20, seed = 9))
})

test_that("kendallTau_bootstrap needs at least two rankings", {
  expect_error(kendallTau_bootstrap(matrix(1:3, 1), matrix(3:1, 1)),
               "at least two")
})
