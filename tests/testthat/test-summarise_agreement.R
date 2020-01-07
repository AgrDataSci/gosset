context("test-summarise_agreement")

library("gosset")
data("breadwheat", package = "gosset")

R <- rank_tricot(data = breadwheat,
                 items = c(1:3),
                 input = c(18:19))


compare <- list()

compare[[1]] <- rank_tricot(data = breadwheat,
                            items = c("variety_a", "variety_b", "variety_c"),
                            input = c("germination_best","germination_worst"))

compare[[2]] <- rank_tricot(data = breadwheat,
                            items = c("variety_a", "variety_b", "variety_c"),
                            input = c("grainquality_best", "grainquality_worst"))


compare[[3]] <- rank_tricot(data = breadwheat,
                            items = c("variety_a", "variety_b", "variety_c"),
                            input = c("yield_best", "yield_worst"))


labels <- c("Germination", "Grain quality", "Yield")

test_that("works with class rankings ", {
  
  a <-
    summarise_agreement(R,
              compare.to = compare,
              labels = labels)
  
  a <- is.data.frame(a)
  
  expect_equal(a, TRUE)
  
})


G <- rank_tricot(data = breadwheat,
                 items = c(1:3),
                 input = c(18:19), 
                 group = TRUE)


compare <- list()

compare[[1]] <- rank_tricot(data = breadwheat,
                            items = c("variety_a", "variety_b", "variety_c"),
                            input = c("germination_best","germination_worst"),
                            group = TRUE)

test_that("works with grouped_rankings", {
  
  a <-
    summarise_agreement(G,
              compare.to = compare,
              labels = NULL)
  
  a <- is.data.frame(a)
  
  expect_equal(a, TRUE)
  
})

test_that("plot works", {
  
  a <- summarise_agreement(G,
                 compare.to = compare,
                 labels = NULL)
  
  ap <- plot(a)
  
  ap <- "ggplot" %in% class(ap)
  
  expect_equal(ap, TRUE)
  
})



test_that("error different class", {
  
  expect_error(summarise_agreement(R,
                               compare.to = compare,
                               labels = NULL))
  
  
})


