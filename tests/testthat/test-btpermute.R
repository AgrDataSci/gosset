test_that("btpermute() runs when BradleyTerry2 is installed", {
  skip_if_not_installed("BradleyTerry2")
  skip_on_cran()
  data("kenyachoice", package = "gosset")
  # fewer iterations than predictors used to fail with
  # 'subscript out of bounds'
  mod = suppressWarnings(btpermute(contests = kenyachoice$contests,
                                   predictors = kenyachoice$predictors,
                                   n.iterations = 2, seed = 1))
  expect_s3_class(mod, "gosset_btpermute")
})
