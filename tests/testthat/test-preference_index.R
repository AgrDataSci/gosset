# exact Plackett-Luce probabilities by enumerating every order of the
# competitors; an independent reference for the package recursion
pl_top = function(w, i, others) w[i] / (w[i] + sum(w[others]))
pl_bottom_enum = function(w, i, others) {
  perms = function(x) {
    if (length(x) <= 1L) return(list(x))
    out = list()
    for (j in seq_along(x)) for (p in perms(x[-j])) out[[length(out) + 1L]] = c(x[j], p)
    out
  }
  total = 0
  for (ord in perms(others)) {
    denom = w[i] + sum(w[ord])
    p = 1
    for (j in ord) {
      p = p * w[j] / denom
      denom = denom - w[j]
    }
    total = total + p
  }
  total
}

triads = matrix(c(1, 2, 3, 0,
                  3, 1, 2, 0,
                  0, 1, 3, 2,
                  2, 0, 1, 3,
                  1, 3, 0, 2,
                  2, 3, 1, 0),
                nrow = 6, byrow = TRUE,
                dimnames = list(NULL, c("A", "B", "C", "D")))
R = PlackettLuce::as.rankings(triads)

test_that("n counts the sets each item appears in", {
  p = preference_index(R)
  expected = colSums(triads > 0)
  expect_equal(p$n[match(names(expected), p$item)], unname(expected))
})

test_that("top and bottom probabilities match exact enumeration", {
  p = preference_index(R)
  w = stats::coef(PlackettLuce::PlackettLuce(R), log = FALSE)
  w = w / sum(w)
  for (it in names(w)) {
    sets = which(triads[, it] > 0)
    top = mean(vapply(sets, function(r) {
      pl_top(w, it, setdiff(names(w)[triads[r, ] > 0], it))
    }, numeric(1)))
    bottom = mean(vapply(sets, function(r) {
      pl_bottom_enum(w, it, setdiff(names(w)[triads[r, ] > 0], it))
    }, numeric(1)))
    row = p[p$item == it, ]
    expect_equal(row$top / 100, unname(top), tolerance = 1e-10)
    expect_equal(row$bottom / 100, unname(bottom), tolerance = 1e-10)
  }
})

test_that("the recursion agrees with enumeration for larger sets", {
  set.seed(1)
  m = 6
  full = t(replicate(30, sample(seq_len(m))))
  colnames(full) = LETTERS[seq_len(m)]
  Rf = PlackettLuce::as.rankings(full)
  p = preference_index(Rf)
  w = stats::coef(PlackettLuce::PlackettLuce(Rf), log = FALSE)
  w = w / sum(w)
  for (it in names(w)) {
    others = setdiff(names(w), it)
    expect_equal(p$bottom[p$item == it] / 100,
                 unname(pl_bottom_enum(w, it, others)), tolerance = 1e-10)
  }
})

test_that("each complete set sums to one at the top and at the bottom", {
  same = PlackettLuce::as.rankings(matrix(c(1, 2, 3, 2, 1, 3, 3, 2, 1, 1, 3, 2),
                                          ncol = 3, byrow = TRUE,
                                          dimnames = list(NULL, c("A", "B", "C"))))
  p = preference_index(same)
  expect_equal(sum(p$top), 100, tolerance = 1e-10)
  expect_equal(sum(p$bottom), 100, tolerance = 1e-10)
})
