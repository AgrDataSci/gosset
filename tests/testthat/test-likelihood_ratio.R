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
  df = (g1$rank + g2$rank) - all$rank
  expect_equal(lr$deviance, dev, tolerance = 1e-8)
  expect_equal(lr$DF_delta, df)
  expect_equal(lr[["Pr(>Chisq)"]], stats::pchisq(dev, df, lower.tail = FALSE),
               tolerance = 1e-10)
})

test_that("grouped rankings are accepted, as in the documented example", {
  G = PlackettLuce::group(R, index = rep(1:4, each = 2))
  expect_no_error(likelihood_ratio(G, split = c(TRUE, TRUE, FALSE, FALSE)))
})

test_that("df are the difference in ranks when groups differ in ties", {
  # group 1 has ties, group 2 has none, so the largest tie order differs
  # and residual df from PlackettLuce are not additive across groups
  Rt = PlackettLuce::as.rankings(matrix(c(1, 1, 2,
                                          2, 1, 1,
                                          1, 2, 2,
                                          1, 2, 3,
                                          1, 2, 3,
                                          2, 1, 3,
                                          3, 1, 2,
                                          2, 3, 1),
                                        ncol = 3, byrow = TRUE,
                                        dimnames = list(NULL, c("A", "B", "C"))))
  split = rep(c("g1", "g2"), each = 4)
  lr = likelihood_ratio(Rt, split = split)
  all = PlackettLuce::PlackettLuce(Rt)
  g1 = PlackettLuce::PlackettLuce(Rt[1:4, ])
  g2 = PlackettLuce::PlackettLuce(Rt[5:8, ])
  df_rank = (g1$rank + g2$rank) - all$rank
  df_resid = all$df.residual - (g1$df.residual + g2$df.residual)
  expect_false(df_rank == df_resid)
  expect_equal(lr$DF_delta, df_rank)
  dev = -2 * (all$loglik - (g1$loglik + g2$loglik))
  expect_equal(lr[["Pr(>Chisq)"]],
               stats::pchisq(dev, df_rank, lower.tail = FALSE),
               tolerance = 1e-10)
})
