context("test-rank_PL")

library("gosset")
library("PlackettLuce")
# compare it with the rankings given by to_rankings
load("../tricot.rda")


beans <- within(tricot, {
  best <- match(best, c("A", "B", "C"))
  worst <- match(worst, c("A", "B", "C"))
  middle <- 6 - best - worst
})

varieties <- as.matrix(beans[c("variety_a", 
                               "variety_b", 
                               "variety_c")])

n <- nrow(beans)
beans <- within(beans, {
  best <- varieties[cbind(seq_len(n), best)]
  worst <- varieties[cbind(seq_len(n), worst)]
  middle <- varieties[cbind(seq_len(n), middle)]
})

lab <- c("Local", sort(unique(as.vector(varieties))))
R <- as.rankings(beans[c("best", "middle", "worst")],
                 input = "ordering", labels = lab)

paired <- list()
for (id in c("a", "b", "c")){
  ordering <- matrix("Local", nrow = n, ncol = 2)
  worse <- beans[[paste0("var_", id)]] == "Worse"
  ## put trial variety in column 1 when it is not worse than local
  ordering[!worse, 1] <- beans[[paste0("variety_", id)]][!worse]
  ## put trial variety in column 2 when it is worse than local
  ordering[worse, 2] <- beans[[paste0("variety_", id)]][worse]
  paired[[id]] <- ordering
}

paired <- lapply(paired, as.rankings, input = "ordering", labels = lab)
R <- rbind(R, paired[["a"]], paired[["b"]], paired[["c"]])

R <- grouped_rankings(R, rep(1:n, 4))

R <- R[1:length(R), , as.grouped_rankings = FALSE]

# rankings produced by to_rankings
G <- rank_PL(data = tricot,
                 items = c("variety_a","variety_b","variety_c"),
                 input = c("best","worst"),
                 type = "tricot",
                 add.rank = tricot[c("var_a","var_b","var_c")],
                 grouped.rankings = TRUE)

G <- G[1:length(G), , as.grouped_rankings = FALSE]


testR <- rep(NA, n)

for(i in seq_len(n)) {
  testR[i] <- sum(G[i,] == R[i,])
}

test_that("rankings are equal", {
  expect_equal(sum(testR == 11), n)
})


