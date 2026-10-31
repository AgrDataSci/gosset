# Run after installing THIS branch of gosset:
# R CMD INSTALL .
# Rscript dev/audits/2026-10-regression-checks.R
# This script has not been executed in the authoring environment.

stopifnot(requireNamespace("gosset", quietly = TRUE))
stopifnot(requireNamespace("PlackettLuce", quietly = TRUE))

expect_error = function(expr) {
  result = tryCatch({force(expr); NULL}, error = identity)
  stopifnot(inherits(result, "error"))
}

x = data.frame(
  id = rep(c("P1", "P2"), each = 3L),
  item = rep(c("A", "B", "C"), 2L),
  rank = c(1L, 2L, 3L, 3L, 2L, 1L),
  stringsAsFactors = FALSE
)
r = gosset::rank_tricot2(x, id = "id", items = "item",
                         input = "rank", strict = TRUE)
stopifnot(inherits(r, "rankings"), nrow(r) == 2L, ncol(r) == 3L)
stopifnot(isTRUE(all.equal(as.matrix(r), matrix(
  c(1, 2, 3, 3, 2, 1), nrow = 2, byrow = TRUE
), check.attributes = FALSE)))
r_by_index = gosset::rank_tricot2(x, id = 1, items = 2, input = 3,
                                   strict = TRUE)
stopifnot(isTRUE(all.equal(as.matrix(r), as.matrix(r_by_index))))

dup = rbind(x, x[1, , drop = FALSE])
expect_error(gosset::rank_tricot2(dup, "id", "item", "rank", strict = TRUE))
missing = x
missing$rank[1] = NA_integer_
expect_error(gosset::rank_tricot2(missing, "id", "item", "rank", strict = TRUE))
ties = x
ties$rank[2] = 1L
expect_error(gosset::rank_tricot2(ties, "id", "item", "rank", strict = TRUE))
bad = x
bad$rank[2] = 7L
expect_error(gosset::rank_tricot2(bad, "id", "item", "rank", strict = TRUE))
expect_error(gosset::rank_tricot2(x[FALSE, ], "id", "item", "rank"))

# Known-answer Plackett-Luce symmetry. Balanced opposite rankings have
# equal worth; each item should have equal top/bottom probabilities.
p = gosset::preference_index(r)
stopifnot(nrow(p) == 3L, all(is.finite(p$top)),
          all(is.finite(p$bottom)), all(p$top >= 0 & p$top <= 100),
          all(p$bottom >= 0 & p$bottom <= 100))

# Exact probabilities for three items under equal worths:
# each top and bottom probability is 1/3. The symmetric input above
# is not guaranteed to yield equal estimated worths for all three
# items in all model parametrizations, so test this property only
# against independent model estimates in the next validation stage.

# Invalid likelihood-ratio split vectors must not be silently accepted.
expect_error(gosset::likelihood_ratio(r, split = c("A", NA)))
expect_error(gosset::likelihood_ratio(r, split = "A"))
cat("gosset regression checks completed successfully.\n")
print(sessionInfo())
