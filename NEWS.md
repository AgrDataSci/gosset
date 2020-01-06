gosset 0.1.7 (2020-01-06)
=========================

### NEW FEATURES

* Classify main functions into families

### CHANGES IN BEHAVIOUR

* Rename functions `agreement()`, `dominance()`, `favourite()`, and `victories()` with the tag `recap` and add it into recap functions


gosset 0.1.6 (2019-01-10)
=========================

### NEW FEATURES

* `.get_timeseries()` now implements the Euclidean method to search for its nearest neighbour in the nasapower output. This optmises the function and reduce dependencies
* ranking functions now implements internal processess to decode ranks and reduce dependencies
* `pseudoR2()` as a S3 method for objects of class "glm", "gnm", "bttree" and "pltree"
* `kendallTau()` as S3 method for objects of class "numeric", "matrix", "data.frame", "rankings", "grouped_rankings"


gosset 0.1.4 (2019-12-01)
=========================

### NEW FEATURES

* Function to summarise results from pairwise comparisons, `agreement()`, `dominance()`, `favourite()` and `victories()`
* Increased test coverage

gosset 0.1.3 (2019-11-15)
=========================

* Successfull migration from **ClimMobTools** to **gosset**

gosset 0.1.2 (2019-10-21)
=========================

### NEW FEATURES

* Migrating functions from **ClimMobTools** to **gosset**

### CHANGES IN BEHAVIOUR

* `predict()` method for crossvalidation and `rank_decimal()` deprecated
* `rank_numeric()` and `rank_tricot()` replaces `rank_PL()`. NO warning message added

gosset 0.1.1 (2019-08-21)
=========================

### NEW FEATURES

* `predict()` method for crossvalidation objects
* `pseudoR2()` for object of class "pltree" extracts null loglik from PlackettLuce
* replace `to_rankings()` by `rank_PL()`. Warning message added
* replace `num2rank()` by `rank_decimal()`. No warning message added
* replace `to_paircomp()` by `rank_paircomp()`. No warning message added


gosset 0.1.0 (2019-03-05)
=========================

* GitHub-only release of prototype package.