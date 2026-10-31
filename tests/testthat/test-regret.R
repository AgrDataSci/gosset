# two nodes whose ids sort differently as text ("10" before "3") and
# items whose first appearance is not alphabetical
coeffs = data.frame(
  items = rep(c("C", "A", "B"), times = 2),
  estimate = c(0.5, 0.3, 0.2,
               0.1, 0.6, 0.3),
  node = rep(c(3, 10), each = 3)
)

test_that("regret is computed within each node and stays with its item", {
  r = regret(coeffs, values = "estimate", items = "items", group = "node",
             bootstrap = FALSE, normalize = FALSE)
  # node 3: best 0.5 -> regrets C 0, A 0.2, B 0.3
  # node 10: best 0.6 -> regrets C 0.5, A 0, B 0.3
  expect_equal(r$worst_regret[r$items == "C"], 0.5)
  expect_equal(r$worst_regret[r$items == "A"], 0.2)
  expect_equal(r$worst_regret[r$items == "B"], 0.3)
  expect_equal(r$regret[r$items == "B"], 0.3^2 + 0.3^2)
  expect_equal(r$worth[r$items == "C"], mean(c(0.5, 0.1)))
})

test_that("standard errors belong to the right item", {
  r = regret(coeffs, values = "estimate", items = "items", group = "node",
             bootstrap = FALSE, normalize = FALSE)
  se = function(x) stats::sd(x) / sqrt(length(x))
  expect_equal(r$worthSE[r$items == "C"], se(c(0.5, 0.1)))
  expect_equal(r$worthSE[r$items == "A"], se(c(0.3, 0.6)))
})

test_that("equal node weights reproduce the unweighted results", {
  r0 = regret(coeffs, values = "estimate", items = "items", group = "node",
              bootstrap = FALSE, normalize = FALSE)
  r1 = regret(coeffs, values = "estimate", items = "items", group = "node",
              node.weights = c("3" = 0.5, "10" = 0.5),
              bootstrap = FALSE, normalize = FALSE)
  expect_equal(r1, r0)
})

test_that("node weights shift worth and regret towards the larger node", {
  r = regret(coeffs, values = "estimate", items = "items", group = "node",
             node.weights = c("3" = 0.9, "10" = 0.1),
             bootstrap = FALSE, normalize = FALSE)
  # worth of C: 0.9 * 0.5 + 0.1 * 0.1
  expect_equal(r$worth[r$items == "C"], 0.9 * 0.5 + 0.1 * 0.1)
  # squared regret of C: n * weighted mean = 2 * (0.9 * 0^2 + 0.1 * 0.5^2)
  expect_equal(r$regret[r$items == "C"], 2 * (0.1 * 0.5^2))
  # worst regret is a worst case and is not weighted
  expect_equal(r$worst_regret[r$items == "C"], 0.5)
})

test_that("invalid node weights are rejected", {
  expect_error(regret(coeffs, values = "estimate", items = "items", group = "node",
                      node.weights = c(0.5, 0.5), bootstrap = FALSE))
  expect_error(regret(coeffs, values = "estimate", items = "items", group = "node",
                      node.weights = c("3" = -1, "10" = 1), bootstrap = FALSE))
})
