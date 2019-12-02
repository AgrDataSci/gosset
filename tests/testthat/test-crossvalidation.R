context("test-crossvalidation")

library("gosset")


# tests here covers:
# GLM model
# further arguments passed to the model frame
# equal mean is used
# no folds provided
test_that("GLM", {
  
  data("airquality")
  
  # same seed but different results when not using 'family' argument
  cv <- crossvalidation(Temp ~ Wind + Solar.R,
                        data = airquality,
                        k = 3,
                        seed = 123,
                        mean.method = "equal",
                        family = poisson())
  
  cv2 <- crossvalidation(Temp ~ Wind + Solar.R,
                        data = airquality,
                        k = 3,
                        seed = 123,
                        mean.method = "equal")
  
  
  
  cvtest <- sum(unlist(cv$coeffs) == unlist(cv2$coeffs))
  
  expect_equal(cvtest, 0)
  
})



# tests here covers:
# PlackettLuce model
# a vector with folds is provided
# stouffer mean is used
# further arguments passed to model frame
test_that("PlackettLuce", {
  
  library("PlackettLuce")
  df <- cbind(id = rep(1:50, each = 5),
              items = rep(LETTERS[1:5], times = 50),
              input = runif(250, 1, 3))
  
  # return an object of class 'rankings'
  R <- rank_numeric(df,
                    items = 2,
                    input = 3,
                    id = 1,
                    group = TRUE)
  
  df <- data.frame(x = rep(1, 50),
                   y = rep(0, 50))
  
  df <- cbind(R, df)
  
  n <- nrow(df)
  
  # take seasons as bins
  k <- 3
  folds <- sample(rep(1:k, times = ceiling(n / k), length.out = n))
  
  cv <- crossvalidation(R ~ .,
                        data = df,
                        k = k,
                        folds = folds)
  
  cv2 <- crossvalidation(R ~ .,
                         data = df,
                         k = k,
                         folds = folds,
                         gamma = TRUE)
  
  cvtest <- sum(unlist(cv$coeffs) == unlist(cv2$coeffs))
  
  expect_equal(cvtest, 0)
  
})

# tests here covers:
# BradleyTerry model
# formula = x ~ .
# foldsize mean is used
# further arguments passed to model frame
test_that("BradleyTerry", {
  
  library("psychotree")
  df <- cbind(id = rep(1:50, each = 5),
              items = rep(LETTERS[1:5], times = 50),
              input = runif(250, 1, 3))
  
  # return an object of class 'rankings'
  R <- rank_numeric(df,
                    items = 2,
                    input = 3,
                    id = 1)
  
  pc <- rank_paircomp(R)
  
  df <- data.frame(x = rep(1, 50),
                   y = rep(0, 50))
  
  df <- cbind(pc, df)
  
  n <- nrow(df)
  
  # different seed different results
  cv <- crossvalidation(pc ~ .,
                        data = df,
                        k = 3, 
                        mean.method = "foldsize",
                        seed = 432)
  
  
  cv2 <- crossvalidation(pc ~ .,
                         data = df,
                         k = 3, 
                         mean.method = "foldsize",
                         seed = 123)
  
  cvtest <- sum(unlist(cv$coeffs) == unlist(cv2$coeffs))
  
  expect_equal(cvtest, 0)
  
})


# print method
test_that("print", {

  data("airquality")

  # same seed but different results when not using 'family' argument
  cv <- crossvalidation(Temp ~ Wind + Solar.R,
                        data = airquality,
                        k = 3,
                        seed = 123,
                        mean.method = "equal",
                        family = poisson())

  expect_output(
    message(print(cv))
  )

})


# test errors 
# mean methods not supported
test_that("invalid mean method", {
  
  data("airquality")
  
 expect_error(
    crossvalidation(Temp ~ Wind + Solar.R,
                        data = airquality,
                        k = 3,
                        mean.method = "xx")
    )
})


# folds different length
test_that("invalid fold length", {
  
  data("airquality")
  
  n <- nrow(airquality) + 50
  k <- 3
  f <- sample(rep(1:k, times = ceiling(n / k), length.out = n))
  
  expect_error(
    crossvalidation(Temp ~ Wind + Solar.R,
                    data = airquality,
                    k = 3,
                    folds =  f)
  )
})


# folds different length
test_that("drop fold", {
  
  data("airquality")
  
  n <- nrow(airquality)
  k <- 5
  f <- sample(rep(1:k, times = ceiling(n / k), length.out = n))
  
  expect_output(
    crossvalidation(Temp ~ Wind + Solar.R,
                    data = airquality,
                    k = k,
                    folds = f,
                    drop.folds = c(1,4))
                )

})

