# Focused source review: ranking, preference and inference

Date: 2026-10-31. Source: `master`, GitHub snapshot `bb5eb75c08fe3ab0f28931c093dcb66dea7c4185`. **Static review only; none of the following is numerically verified in R.**

## Confirmed source behaviors

- `rank_tricot2()` converts long `id,item,rank` records to a matrix with zero for absent items and calls `PlackettLuce::as.rankings()`. It warns and skips rows with NA in the three input fields. Duplicate (id,item) records warn and the last value overwrites earlier values.
- `rank_numeric()` supports wide input, numerical measurements and `ascending`, and can return grouped rankings. A general migration to `rank_tricot2()` therefore needs feature-by-feature tests.
- `preference_index()` fits a PL model, computes top probability from worths, and computes bottom probability by explicitly enumerating permutations of other items in each observed ranking set.
- `likelihood_ratio()` fits pooled and group-specific PL models and calculates an asymptotic chi-square tail probability without explicit pre-fit checks for group sizes or comparison-network structure.
- `crossvalidation()` replaces estimator failures with zeros, and one prediction pathway calls `predict()` on a model-name string instead of a fitted model object. Whether and how these branches are exercised requires runtime tests.

## Performance concern: preference_index()

The number of permutations evaluated by `p_bottom()` is `(m-1)!` for a ranking set of size `m`, and each permutation is iterated over. This is manageable for triads, but scales poorly for larger consumer-ranking sets. **Do not optimize by changing the estimand.** First establish reference values for 2, 3, 4 and 5 items, then benchmark an exact dynamic-programming alternative or a documented approximation with error bounds.

## Proposed next test matrix

| Function | Known-answer checks | Boundary and adversarial checks |
|---|---|---|
| `rank_tricot2` | 3-item rankings, item ordering, incomplete blocks | duplicate conflicting ranks, NA, factors, invalid rank, ties, empty input |
| `rank_tricot` | A/B/C best-worst encoding on documented beans example | unrecognized letters, NA, ties, additional local item |
| `rank_numeric` | numeric values, ascending and descending, wide/long | tied measurements, noninteger IDs, missing item |
| `preference_index` | equal worth symmetry, 2- and 3-item exact probabilities | absent items, no connected worth, large ranking sets |
| `likelihood_ratio` | no-split null, known nested model | empty split, disconnected strata, small-sample type-I error |
| `crossvalidation` | held-out predictions, independent fold aggregation | fit failure, blocked folds, list folds, random-state preservation |

## Safety and release gates

1. No production changes until tests have been executed in R and reviewed.
2. Failures must be visible, never silently converted into plausible numerical results.
3. Separate methodological changes from backward-compatible bug fixes.
4. Pin dependency versions, record `sessionInfo()`, and compare downstream `ClimMob-analysis` outputs.
5. All new R code uses `=` for assignment.

See `2026-10-ranking-probes.R` for the first executable test fixtures.
