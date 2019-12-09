# gosset 0.1-5

# New features
* `.get_timeseries` now implements the Euclidean method to look for its nearest neighbour in the nasapower output. This optmises the functions and reduce dependencies
* ranking functions now implements internal processess to decode ranks and reduce dependencies


# gosset 0.1-4

## New features

* Function to summarise results from pairwise comparisons, `agreement`, `dominance`, `favourite` and `victories`
* Increased test coverage

# gosset 0.1-3

* Successfull migration from **ClimMobTools** to **gosset**

# gosset 0.1-2

## New features

* Migrating functions from **ClimMobTools** to **gosset**

## Changes in behaviour

* `predict` method for crossvalidation and `rank_decimal` deprecated.
* `rank_numeric` and `rank_tricot ` replaces `rank_PL`. NO warning message added.

# gosset 0.1-1

## New features

* `predict` method for crossvalidation objects.
* `pseudoR2` for object of class "pltree" extracts null loglik from PlackettLuce.
* replace `to_rankings` by `rank_PL`. Warning message added
* replace `num2rank` by `rank_decimal`. No warning message added
* replace `to_paircomp` by `rank_paircomp`. No warning message added


# gosset 0.1-0

* GitHub-only release of prototype package.