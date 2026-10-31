test_that("folds stay aligned with rows when missing values are dropped", {
  skip_if_not_installed("gnm")
  d = airquality
  complete = stats::complete.cases(d[, c("Temp", "Wind", "Solar.R")])
  folds = rep(1:3, length.out = nrow(d))
  cv = suppressMessages(crossvalidation(Temp ~ Wind + Solar.R, data = d,
                                        k = 3, folds = folds,
                                        family = poisson()))
  expect_equal(cv$raw$folds, folds[complete])
  expect_equal(nrow(cv$raw$data), sum(complete))
})

test_that("a seed gives reproducible folds without changing the caller's stream", {
  skip_if_not_installed("gnm")
  d = airquality[stats::complete.cases(airquality), ]
  set.seed(42)
  before = .Random.seed
  cv1 = suppressMessages(crossvalidation(Temp ~ Wind, data = d, k = 3,
                                         seed = 7, family = poisson()))
  expect_identical(.Random.seed, before)
  cv2 = suppressMessages(crossvalidation(Temp ~ Wind, data = d, k = 3,
                                         seed = 7, family = poisson()))
  expect_identical(cv1$raw$folds, cv2$raw$folds)
})

test_that("folds given as a list of training rows are supported", {
  skip_if_not_installed("gnm")
  d = airquality[stats::complete.cases(airquality), ]
  n = nrow(d)
  idx = rep(1:3, length.out = n)
  folds = lapply(1:3, function(i) which(idx != i))
  cv = suppressMessages(crossvalidation(Temp ~ Wind, data = d,
                                        folds = folds, family = poisson()))
  expect_equal(nrow(cv$raw$estimators), 3L)
})

test_that("invalid k and folds are rejected", {
  d = airquality[stats::complete.cases(airquality), ]
  expect_error(crossvalidation(Temp ~ Wind, data = d, k = 1))
  expect_error(crossvalidation(Temp ~ Wind, data = d, k = 3,
                               folds = rep(1:2, length.out = nrow(d))))
})

test_that("Kendall's tau is computed on held-out data for Plackett-Luce trees", {
  skip_on_cran()
  data("beans", package = "PlackettLuce")
  G = rank_tricot(data = beans, items = c(1:3), input = c(4:5),
                  additional.rank = beans[c(6:8)], group = TRUE)
  d = cbind(G, beans)
  folds = as.integer(as.factor(d$season))
  k = max(folds)
  cv = suppressMessages(crossvalidation(G ~ maxTN, data = d, k = k,
                                        folds = folds, minsize = 100))
  kt = cv$raw$estimators$kendallTau
  expect_length(kt, k)
  expect_true(all(is.finite(kt)))
  expect_true(all(kt >= -1 & kt <= 1))
})
