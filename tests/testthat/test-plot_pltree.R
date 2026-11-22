data("beans", package = "PlackettLuce")

G = rank_tricot(data = beans,
                items = c(1:3),
                input = c(4:5),
                group = TRUE,
                additional.rank = beans[c(6:8)])

pld = cbind(G, beans[, c("maxTN", "season", "lon")])

tree = PlackettLuce::pltree(G ~ maxTN + season + lon, data = pld)

skip_if(length(tree) == 1L, "tree has no split")

test_that("tree branches are drawn without ggparty", {
  p = build_tree_branches(tree)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("terminal nodes align with the node panels", {
  p = build_tree_branches(tree)
  edges = p$layers[[1]]$data
  ends = edges$xend[!edges$to %in% edges$from]
  n = length(partykit::nodeids(tree, terminal = TRUE))
  expect_equal(sort(ends), (seq_len(n) - 0.5) / n)
})

test_that("inner nodes show the splitting variable and node id", {
  p = build_tree_branches(tree)
  inner = p$layers[[3]]$data
  expect_setequal(inner$id, partykit::nodeids(tree)[
    !partykit::nodeids(tree) %in% partykit::nodeids(tree, terminal = TRUE)])
  expect_true(all(inner$splitvar %in% c("maxTN", "season", "lon")))
})

test_that("plot() on a pltree returns a combined plot", {
  p = plot(tree)
  expect_s3_class(p, "patchwork")
  expect_no_error(print(p))
})

test_that("node panels draw without ggplot2 deprecation warnings", {
  p = plot(tree)
  expect_no_warning(print(p))
})
