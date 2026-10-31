# Reproducible audit probes for gosset. Not yet executed in R.
# Run in a development checkout after installing the package:
# Rscript dev/audits/2026-10-ranking-probes.R
# All assignments use =.

stopifnot(requireNamespace("gosset", quietly = TRUE))
stopifnot(requireNamespace("PlackettLuce", quietly = TRUE))

# Independent, small known-answer fixture: two complete 3-item rankings.
x = data.frame(
  id = rep(c("P1", "P2"), each = 3),
  item = rep(c("A", "B", "C"), times = 2),
  rank = c(1, 2, 3, 3, 2, 1),
  stringsAsFactors = FALSE
)
r = gosset::rank_tricot2(x, id = "id", items = "item", input = "rank")
stopifnot(inherits(r, "rankings"))
stopifnot(identical(dim(r), c(2L, 3L)))
stopifnot(isTRUE(all.equal(unclass(as.matrix(r)), unclass(matrix(
  c(1, 2, 3, 3, 2, 1), nrow = 2, byrow = TRUE,
  dimnames = list(c("P1", "P2"), c("A", "B", "C"))
)), check.attributes = FALSE)))

# Incomplete block: absent item must be encoded as unranked (0).
x2 = x[x$id == "P1", , drop = FALSE]
x2 = rbind(x2, data.frame(id = "P2", item = c("A", "B"),
                           rank = c(2, 1)))
r2 = gosset::rank_tricot2(x2, id = "id", items = "item", input = "rank")
stopifnot(as.matrix(r2)["P2", "C"] == 0)

# Duplicate records currently emit a warning. This is NOT validation that
# overwriting the last value is an acceptable scientific default.
dup = rbind(x, x[1, , drop = FALSE])
warning_text = character()
withCallingHandlers(
  gosset::rank_tricot2(dup, id = "id", items = "item", input = "rank"),
  warning = function(w) {
    warning_text <<- c(warning_text, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
stopifnot(any(grepl("duplicate", warning_text, ignore.case = TRUE)))

# Do not run invalid-rank and contradictory-duplicate cases as pass/fail
# expectations until the intended strict/permissive API has been specified.
cat("Ranking audit probes completed. Review results and sessionInfo below.\n")
print(sessionInfo())
