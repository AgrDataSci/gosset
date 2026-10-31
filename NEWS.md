gosset (development version)
=========================

### BUG FIXES

* `crossvalidation()` now reports Kendall's tau for Plackett-Luce trees on the
  held-out test set. Earlier versions compared each model's predictions with
  its own training rankings, so the reported tau was an in-sample measure and
  will be lower, and more honest, from this version on.
* `crossvalidation()` keeps folds aligned with the data when rows are dropped
  for missing values, accepts folds given as a list of training rows without
  failing, stops with an error when a model or a score cannot be computed
  (instead of returning zeros), fits models without requiring the user to
  attach their package, and no longer changes the caller's random number
  stream when `seed` is given.
* `likelihood_ratio()` checks the `split` vector, accepts grouped rankings
  as in its documented example, passes `...` to `PlackettLuce()` and computes
  the p-value with `lower.tail = FALSE`.
* `preference_index()` counts each item once per set. Earlier versions
  inflated `n` by the number of other items in each set and gave larger sets
  more weight in the averages. Sets with a single ranked item are skipped.
  The bottom probability is computed exactly by recursion over subsets.
* `rank_tricot2()` reads factor ranks by their values and gains
  `strict = FALSE`, an opt-in check for missing, duplicated, tied and
  non-consecutive ranks.

* `regret()` assigns each regret to the right item when nodes are not in
  sorted order (for example terminal node 10 before node 3), which affected
  trees with node ids of 10 or more, and reports `worthSE` and `regretSE`
  for the right items.
* `kendallTau()` on matrices weights each row by its number of pairs and
  computes the effective N from the total number of pairs. Results for
  complete triads are unchanged; rows with two, four or more items were
  weighted wrongly before.
* `anova()` for PlackettLuce models no longer rounds the log-likelihood
  difference before computing the statistic, and computes the p-value
  with `lower.tail = FALSE`.
* `rank_tricot()` warns when the same item appears twice in a row.

### TESTS

* Added unit tests in `tests/testthat` for these functions, run by
  `R CMD check`.

gosset 1.5.1 (2025-11-19)
=========================

### NEW FEATURES

* Added `preference_index()` to compute a model-based preference index  
  from Plackett–Luce worth parameters.  
  The function reports, for each item:
  - expected top probability (%),
  - expected bottom probability (%),
  - net winning probability (%), and
  - normalized worth.

* Added `rank_tricot2()` — a redesigned ranking parser for *tricot* data.  
  The function:
  - handles ties and missing values more robustly,  
  - enforces stricter validation of A–B–C syntax,  
  - provides clearer error messages, and  
  - returns rankings in a format consistent with Plackett–Luce models.

### IMPROVEMENTS

* Updated internal utilities in `rank_numeric()` to interface with `rank_tricot2()` for improved performance and error handling.
* Minor internal cleanup (no user-visible behavioural changes).

### BUG FIXES

* None reported for this release.


gosset 1.4 (2024-12-04)
=========================

### IMPROVEMENTS

* manage factor levels in `plot.pltree()`
* introduces the functions `kendallTau_permute()` and `kendallTau_bootstrap()`
* adds S3 method to plot PlackettLuce objects using `plot.PlackettLuce()`
* adds `cassava` to the datasets
* adds `cassava` consumer testing data and new vignette
* improves readability of common bean vignette
* coefficients from `worth_map()` are extracted using `stats::coefficients()` instead of `psychotools::itempar()`
* update the bean vignette


gosset 1.3 (2024-05-26)
=========================

### IMPROVEMENTS

* Implements the function `resample()` to re-sample Plackett-Luce coefficients
* Adds standard errors in `regret()`  
* Adds `pairwise_probs()` to compare worth parameters following the Luce's Axiom

###  BUG FIXES
* Fix the computation of maximized regret in `regret()`. Using `sum(x^2)` instead of `sum(x)^2`


gosset 1.2 (2023-07-03)
=========================

### IMPROVEMENTS

* Implement `anova()` method for PlackettLuce models
* Implement likelihood-ratio test for PlackettLuce models. Function `likelihood_ratio()` 


gosset 1.0 (2023-04-18)
=========================

### IMPROVEMENTS

* `kendallTau()` returns Z-value and p-values

### Changes in behaviour

* Functions `rank_binomial()` and `rank_paircomp()` are renamed as `set_binomialfreq()` and `set_paircomp()`, respectively


gosset 0.7 (2023-02-26)
=========================

### IMPROVEMENTS

* Adds argument `validate.rankings` to `rank_tricot()` to help in identifying possible issues in input tricot ranking like `NA`, ties or letters different than A, B or C
* Adds argument `average` to `kendallTau()` to allow returning the kendall correlation for each entry in a matrix and PlackettLuce ranking
* Adds argument `na.omit` to `kendallTau()` to remove entries with `NA`
* Code is written using ` = ` instead of ` <- ` to avoid typing stress 

###  BUG FIXES

* Fixes an issue in `plot.pltree()` when multi-comparison is required but multicompView is not loaded.

gosset 0.5 (2022-09-23)
=========================

### IMPROVEMENTS

* Fix issues in CRAN cmd checks

gosset 0.4 (2022-06-30)
=========================

### IMPROVEMENTS

* Add new functions `reliability()`, `btpermute()`, `pseudo_rank()`, `kendallW()`
* Split `plot.pltree()` to build branches and panels independently


gosset 0.3 (2022-03-08)
=========================

First version on CRAN 


gosset 0.2.5.9000 (2020-09-16)
=========================

### IMPROVEMENTS

* add argument qve to skip quasi-variance to plot coefficients in `plot_tree()`
* `rank_numeric()` deals with ids of class characters
* argument `ref` can be used in `multicompPL()` to select the reference item
* adjust scale in the `plot()` method for `summarise_agreement()` using the argument scales, where `scales = 100` is to set the scale from 0-100 and `scales = 1` for a scale from 0-1
* plot method for `multcompPL()`

###  BUG FIXES
* Fix an issue in `rank_binomial()` in assigning the ids when argument `disaggregate = TRUE`
* Fix an issue in setting up the permuted formulas in `btpermute()`, with the fix n.formula = n.vars + 1 


gosset 0.2.2 (2020-03-17)
=========================
### NEW FEATURES
* forward selection is added with `forward()`
* `btpermute()` is added for model selection with BTm() 
* abbreviation method in plot methods of summarise_* functions

### IMPROVEMENTS
* No user visible improvements

gosset 0.1.9 (2020-02-08)
=========================

### CHANGES IN BEHAVIOUR

* Functions `rainfall()`, `temperature()`, `ETo()` and `GDD()` migrates to **climatrends**
* Change license to MIT
* Change logo

gosset 0.1.8 (2020-01-16)
=========================

### IMPROVEMENTS

* Write documentation in good practice. Avoid calling the entire package but specific functions.

### CHANGES IN BEHAVIOUR

* argument "ascending" in `rank_numeric()` in restricted to floating point numbers. Integer values are ranked as provided by input.



gosset 0.1.7 (2020-01-06)
=========================

### NEW FEATURES

* Classify main functions into families
* `rainfall()` and `temperature()` now allows for time series indices 

### CHANGES IN BEHAVIOUR

* Rename functions `agreement()`, `dominance()`, `favourite()`, and `victories()` with the tag `summarise` and add it into summarise functions. No warking message provided. New functions are `summarise_agreement()`, `summarise_dominance()`, `summarise_favourite()`, and `summarise_victories()`

* Argument 'index' is removed from `rainfall()` and `temperature()`, 
all indices are given by default



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

