df = data.frame(id = rep(1:2, each = 3),
                items = rep(c("B", "A", "C"), times = 2),
                input = c(2.5, 1.2, 3.1,
                          0.4, 2.2, 1.9),
                stringsAsFactors = FALSE)

test_that("long data give dense rankings, highest value first", {
  R = suppressWarnings(rank_numeric(df, items = 2, input = 3, id = 1))
  m = unclass(R)
  expect_equal(colnames(m), c("A", "B", "C"))
  expect_equal(unname(m[1, ]), c(3, 2, 1))
  expect_equal(unname(m[2, ]), c(1, 3, 2))
})

test_that("ascending reverses the order", {
  R = suppressWarnings(rank_numeric(df, items = 2, input = 3, id = 1,
                                     ascending = TRUE))
  m = unclass(R)
  expect_equal(unname(m[1, ]), c(1, 2, 3))
  expect_equal(unname(m[2, ]), c(3, 1, 2))
})

test_that("wide data give the same rankings as long data", {
  wide = data.frame(item_1 = c("B", "B"), 
                    item_2 = c("A", "A"), 
                    item_3 = c("C", "C"),
                    val_1 = c(2.5, 0.4), 
                    val_2 = c(1.2, 2.2), 
                    val_3 = c(3.1, 1.9),
                    stringsAsFactors = FALSE)
  suppressWarnings({
    Rw = rank_numeric(wide, items = 1:3, input = 4:6)
    Rl = rank_numeric(df, items = 2, input = 3, id = 1)
  })
  expect_equal(unname(unclass(Rw)), unname(unclass(Rl)))
})

test_that("missing items in wide data are left as not ranked", {
  wide = data.frame(item_1 = c("A", "A"), 
                    item_2 = c("B", "C"), 
                    item_3 = c("C", NA),
                    val_1 = c(1, 2), 
                    val_2 = c(2, 1), 
                    val_3 = c(3, NA),
                    stringsAsFactors = FALSE)
  R = suppressWarnings(rank_numeric(wide, items = 1:3, input = 4:6))
  m = unclass(R)
  expect_equal(colnames(m), c("A", "B", "C"))
  expect_equal(unname(m[2, "B"]), 0)
})

test_that("group = TRUE returns grouped rankings", {
  G = suppressWarnings(rank_numeric(df, items = 2, input = 3, id = 1,
                                     group = TRUE))
  expect_s3_class(G, "grouped_rankings")
})

test_that("duplicated items for one id are rejected", {
  dup = rbind(df, df[1, ])
  expect_error(suppressWarnings(rank_numeric(dup, items = 2, input = 3, 
                                             id = 1)))
})
