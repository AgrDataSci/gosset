R = PlackettLuce::as.rankings(matrix(c(1, 2, 3,
                                       2, 1, 3,
                                       1, 3, 2,
                                       3, 1, 2,
                                       2, 3, 1,
                                       1, 2, 3,
                                       3, 2, 1,
                                       2, 1, 3),
                                     ncol = 3, byrow = TRUE,
                                     dimnames = list(NULL, c("A", "B", "C"))))

test_that("invalid split vectors are rejected", {
  expect_error(likelihood_ratio(R, split = rep("a", 7)))
  expect_error(likelihood_ratio(R, split = c(rep("a", 7), NA)))
  expect_error(likelihood_ratio(R, split = rep("a", 8)))
})

test_that("the statistic matches separately fitted models", {
  split = rep(c("g1", "g2"), each = 4)
  lr = likelihood_ratio(R, split = split)
  all = PlackettLuce::PlackettLuce(R)
  g1 = PlackettLuce::PlackettLuce(R[1:4, ])
  g2 = PlackettLuce::PlackettLuce(R[5:8, ])
  dev = -2 * (all$loglik - (g1$loglik + g2$loglik))
  df = all$df.residual - (g1$df.residual + g2$df.residual)
  expect_equal(lr$deviance, dev, tolerance = 1e-8)
  expect_equal(lr$DF_delta, df)
  expect_equal(lr[["Pr(>Chisq)"]], stats::pchisq(dev, df, lower.tail = FALSE),
               tolerance = 1e-10)
})

test_that("grouped rankings are accepted, as in the documented example", {
  G = PlackettLuce::group(R, index = rep(1:4, each = 2))
  expect_no_error(likelihood_ratio(G, split = c(TRUE, TRUE, FALSE, FALSE)))
})
