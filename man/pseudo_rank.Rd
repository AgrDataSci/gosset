% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pseudo_rank.R
\name{pseudo_rank}
\alias{pseudo_rank}
\title{Add pseudo-rank to missing values}
\usage{
pseudo_rank(object, ...)
}
\arguments{
\item{object}{a matrix or PlackettLuce rank}

\item{...}{additional arguments passed to methods}
}
\value{
a matrix or PlackettLuce rank
}
\description{
Add pseudo-rank to missing values
}
\examples{
library("PlackettLuce")
R = matrix(c(1, 2, 0, 0,
              4, 1, 0, 3,
              2, 1, 0, 3,
              1, 2, 0, 0,
              2, 1, 0, 0,
              1, 0, 0, 2), nrow = 6, byrow = TRUE)
colnames(R) = c("apple", "banana", "orange", "pear")

# summary(PlackettLuce(R))

R = pseudo_rank(R)

summary(PlackettLuce(R))
}
