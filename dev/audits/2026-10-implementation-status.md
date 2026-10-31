# gosset modernization: implementation and validation status

Date: 2026-10-31. Branch: `audit/scientific-validation-2026`.

## Changes made (proposed, not released)

- `R/rank_tricot2.R`: adds opt-in `strict = TRUE` input checks; default `FALSE` retains warning-based duplicate and missing handling. Supports named and positional column selectors. In strict mode requires a complete 1..n rank sequence per participant. This is a **policy choice**: partial rankings or tied rankings may be valid for some studies and must not be passed through strict mode without checking their intended interpretation.
- `R/preference_index.R`: replaces permutation enumeration for bottom probabilities with memoized exact recursion. Complexity is exponential in set size rather than factorial; this is not a scalable method for arbitrarily large sets. Independently derived 3-item probability checks are included.
- `R/crossvalidation.R`: validates folds, stops on failed fits and estimator failures, and removes an unused prediction pathway. **The existing Plackett-Luce tree Kendall-tau evaluation and fold score aggregation have not been scientifically validated.**
- `R/likelihood_ratio.R`: checks split alignment and degrees of freedom; forwards fitting options. **Asymptotic chi-square validity under sparse or disconnected groups is not established.**
- `dev/audits/2026-10-regression-checks.R`: regression and known-answer checks.
- `.github/workflows/scientific-regression.yml`: proposed GitHub Actions workflow.

## Outstanding release blockers

1. **R has not been executed in the authoring environment**; no claim of passing tests, benchmark gains, numerical equivalence or clean R CMD check is made.
2. Run the branch in CI and resolve any failures. Check actual workflow activation and permissions.
3. Compare against CRAN 1.5.1 and master on existing datasets; verify no silent output changes.
4. Test long, wide, grouped, tied, partial, missing, and duplicated ranking formats across `rank_tricot`, `rank_tricot2`, `rank_numeric`.
5. Independently validate cross-validation out-of-sample predictions, fold-level scores, and no leakage. Check list-based folds and R RNG side effects.
6. Validate likelihood-ratio calibration and small-sample behavior with simulations.
7. Benchmark preference index at realistic 3, 5, 10 and larger items, inspect memory consumption.
8. Review `ClimMob-analysis` downstream compatibility and update package documentation, changelog and release notes once behavior is confirmed.
9. Complete the remaining exported-function inventory and scientific review before describing the *entire package* as modernized.

**Merge recommendation: do not merge until the runtime and scientific validation gates above are met.**
