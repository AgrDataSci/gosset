context("test-akaike_weights")


data("airquality")

# try three model approaches
mod1 <- glm(Temp ~ 1,
            data = airquality,
            family = poisson())

mod2 <- glm(Temp ~ Ozone,
            data = airquality,
            family = poisson())

mod3 <- glm(Temp ~ Ozone + Solar.R,
            data = airquality,
            family = poisson())

# models AICs together in a single vector
models <- c(mod1 = AIC(mod1),
            mod2 = AIC(mod2),
            mod3 = AIC(mod3))



test_that("mod3 best", {
  
  # calculate akaike weights
  aw <- akaike_weights(models)
  
  # the higher the better
  aw <- names(models[which.max(aw$akaike_weights)])
  
  aw <- aw == "mod3"
  
  expect_equal(aw, TRUE)
  
})


