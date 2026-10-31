tricot = data.frame(
  id = rep(c("P1", "P2"), each = 3L),
  item = rep(c("A", "B", "C"), 2L),
  rank = c(1L, 2L, 3L, 3L, 2L, 1L),
  stringsAsFactors = FALSE
)

test_that("valid tricot data give the expected rankings", {
  r = rank_tricot2(tricot, id = "id", items = "item", input = "rank")
  expect_s3_class(r, "rankings")
  expect_equal(dim(unclass(r)), c(2L, 3L))
  expect_equal(unname(unclass(r)[1, ]), c(1, 2, 3))
  expect_equal(unname(unclass(r)[2, ]), c(3, 2, 1))
})

test_that("numeric and character column selectors agree", {
  r1 = rank_tricot2(tricot, id = "id", items = "item", input = "rank")
  r2 = rank_tricot2(tricot, id = 1, items = 2, input = 3)
  expect_equal(unclass(r1), unclass(r2))
})

test_that("factor ranks are read as their values, not their level codes", {
  f = tricot
  f$rank = factor(f$rank, levels = c("3", "2", "1"))
  r = rank_tricot2(f, id = "id", items = "item", input = "rank")
  expect_equal(unname(unclass(r)[1, ]), c(1, 2, 3))
})

test_that("strict mode is off by default and keeps legacy warnings", {
  dup = rbind(tricot, tricot[1, , drop = FALSE])
  expect_warning(rank_tricot2(dup, "id", "item", "rank"), "duplicate")
  miss = tricot
  miss$rank[1] = NA
  expect_warning(rank_tricot2(miss, "id", "item", "rank"), "NA")
})

test_that("strict mode rejects malformed records", {
  dup = rbind(tricot, tricot[1, , drop = FALSE])
  expect_error(rank_tricot2(dup, "id", "item", "rank", strict = TRUE))
  miss = tricot
  miss$rank[1] = NA
  expect_error(rank_tricot2(miss, "id", "item", "rank", strict = TRUE))
  tie = tricot
  tie$rank[2] = 1L
  expect_error(rank_tricot2(tie, "id", "item", "rank", strict = TRUE), "Tied")
  gap = tricot
  gap$rank[3] = 7L
  expect_error(rank_tricot2(gap, "id", "item", "rank", strict = TRUE), "consecutive")
  zero = tricot
  zero$rank[3] = 0L
  expect_error(rank_tricot2(zero, "id", "item", "rank", strict = TRUE))
})

test_that("bad arguments are rejected", {
  expect_error(rank_tricot2(as.matrix(tricot), "id", "item", "rank"))
  expect_error(rank_tricot2(tricot, "id", "item", "rank", strict = NA))
  expect_error(rank_tricot2(tricot, "id", "nope", "rank"))
  expect_error(rank_tricot2(tricot, 1, 9, 3))
  expect_error(rank_tricot2(tricot[FALSE, ], "id", "item", "rank"))
})
