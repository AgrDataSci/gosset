test_that("anova statistic and p-value are unrounded and upper-tail", {
  R = PlackettLuce::as.rankings(matrix(c(1, 2, 3, 1, 3, 2, 2, 1, 3, 1, 2, 3),
                                       ncol = 3, byrow = TRUE,
                                       dimnames = list(NULL, c("A", "B", "C"))))
  mod = PlackettLuce::PlackettLuce(R)
  a = anova(mod)
  stat = 2 * (mod$loglik - mod$null.loglik)
  df = mod$df.null - mod$df.residual
  expect_equal(a$Statistic[2], stat)
  expect_equal(a[["Pr(>Chisq)"]][2], stats::pchisq(stat, df, lower.tail = FALSE))
})
