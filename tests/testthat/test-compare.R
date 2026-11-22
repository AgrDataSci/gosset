test_that("compare() returns a ggplot", {
  set.seed(1)
  p = compare(runif(10), runif(10))
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})
