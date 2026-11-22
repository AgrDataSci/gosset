test_that("perfect agreement gives 1 and reversed ranks give 0", {
  pred = matrix(c(1, 2, 3,
                  3, 1, 2), nrow = 2, byrow = TRUE)
  expect_equal(kendallW(pred, pred), 1)
  expect_equal(kendallW(matrix(c(1, 2, 3), nrow = 1),
                        matrix(c(3, 2, 1), nrow = 1)), 0)
})

test_that("ties are corrected as in the Kendall W formula", {
  # ranks 1, 2, 3 against 1.5, 1.5, 3
  # S = 6.5, ties = 6, W = 12 * 6.5 / (4 * 24 - 2 * 6) = 13 / 14
  expect_equal(kendallW(matrix(c(1, 2, 3), nrow = 1),
                        matrix(c(1, 1, 2), nrow = 1)), 13 / 14)
})

test_that("items not ranked (0) are dropped", {
  # the participant ranked items 1 to 3 in the predicted order, 
  # item 4 was not ranked and must not affect W
  pred = matrix(c(1, 2, 3, 4,
                  4, 1, 2, 3), nrow = 2, byrow = TRUE)
  obs = matrix(c(1, 2, 3, 0,
                 0, 1, 2, 3), nrow = 2, byrow = TRUE)
  expect_equal(kendallW(pred, obs), 1)
})

test_that("rows with fewer than two ranked items are skipped", {
  pred = matrix(c(1, 2, 3,
                  1, 2, 3), nrow = 2, byrow = TRUE)
  obs = matrix(c(1, 0, 0,
                 1, 2, 3), nrow = 2, byrow = TRUE)
  expect_equal(kendallW(pred, obs), 1)
  expect_true(is.na(kendallW(pred[1, , drop = FALSE], obs[1, , drop = FALSE])))
})

test_that("results match DescTools on complete rankings", {
  skip_if_not_installed("DescTools")
  set.seed(1)
  pred = t(replicate(20, sample(1:5)))
  obs = t(replicate(20, sample(c(1, 1, 2, 3, 4))))
  old = mean(vapply(seq_len(nrow(pred)), function(i) {
    DescTools::KendallW(data.frame(pred = pred[i, ], obs = obs[i, ]),
                        correct = TRUE)
  }, numeric(1)))
  expect_equal(kendallW(pred, obs), old)
})

test_that("x and y must have the same dimensions", {
  expect_error(kendallW(matrix(1:6, 2), matrix(1:4, 2)))
})
