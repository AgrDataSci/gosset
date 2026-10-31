# gosset scientific validation audit (initial findings)

**Date:** 2026-10-31. **Status:** preliminary source review; R tests and numerical results have **not** been executed or independently validated. No production functions are changed.

Source: AgrDataSci/gosset master, GitHub source snapshot `bb5eb75c08fe3ab0f28931c093dcb66dea7c4185`; published comparison CRAN 1.5.1.

## Source findings and tests to prioritize

| ID | Priority | Evidence | Required verification |
|---|---|---|---|
| GOS-001 | High | `R/rank_tricot2.R` warns about missing records and duplicate (id,item) pairs; duplicates are overwritten by the last value | Test conflicting duplicates, NA, ties, invalid numeric ranks, malformed triads and strict/permissive behavior |
| GOS-002 | High | `NEWS.md` 1.5.1 describes stricter A/B/C validation than is apparent from the source implementation of `rank_tricot2()` | Resolve documentation-versus-implementation discrepancy, distinguishing positional A/B/C codes from numeric ranks |
| GOS-003 | High | `R/crossvalidation.R` substitutes zero-valued estimator matrices after certain failures | Reproduce failures and ensure unsuccessful fits are not reported as valid zero scores |
| GOS-004 | High | `R/crossvalidation.R` passes the model-selector string to one `predict()` pathway, rather than the fitted model list | Test held-out predictions for each supported model family |
| GOS-005 | Medium | `R/rank_numeric.R` recommends `rank_tricot2()` while supporting numeric measurements, ascending ranks, wide inputs and grouping | Test feature parity before migration or deprecation |
| GOS-006 | High | `R/likelihood_ratio.R` fits pooled/split models without explicit checks for split alignment, sparse strata or identifiability | Test degrees of freedom, item connectivity, convergence, small samples and null type-I error |
| GOS-007 | Medium | A testthat runner was found under `dev/tests`; complete test coverage is not established | Inventory test files and ensure tests are run by R CMD check |
| GOS-008 | Medium | `crossvalidation()` calls `set.seed()` even when the caller did not provide a seed | Test reproducibility and impact on caller RNG state |

GOS-003, GOS-004 and GOS-006 are **source-review hypotheses**, not proven numerical defects.

## Validation plan

1. Inventory every exported function, its dependencies, source, documentation, tests and downstream consumers.
2. Establish known-answer tests for rank parsing, incomplete-block representation, missing data, ties and duplicates.
3. Compare `rank_tricot()`, `rank_tricot2()` and `rank_numeric()` on inputs that are genuinely equivalent, preserving documented differences.
4. Validate PL inference, preference indices, model comparison, likelihood-ratio tests and uncertainty against independent reference implementations and simulations.
5. Test cross-validation with blocked folds, correct held-out predictions, failed fits, random-seed reproducibility and no participant leakage.
6. Compare CRAN 1.5.1, master and devel behavior, and check downstream `ClimMob-analysis` compatibility.
7. Run `R CMD check` and CI, then benchmark only after numerical equivalence has been established.

**Acceptance:** reproducible R tests, independent numerical checks, documented assumptions and API changes, clean CI, and no silently masked model failures. Use `=` for R assignment.

## Source links

- [Ranking parser](https://github.com/AgrDataSci/gosset/blob/bb5eb75c08fe3ab0f28931c093dcb66dea7c4185/R/rank_tricot2.R)
- [Numeric ranking](https://github.com/AgrDataSci/gosset/blob/bb5eb75c08fe3ab0f28931c093dcb66dea7c4185/R/rank_numeric.R)
- [Cross-validation](https://github.com/AgrDataSci/gosset/blob/bb5eb75c08fe3ab0f28931c093dcb66dea7c4185/R/crossvalidation.R)
- [Likelihood-ratio test](https://github.com/AgrDataSci/gosset/blob/bb5eb75c08fe3ab0f28931c093dcb66dea7c4185/R/likelihood_ratio.R)
- [Release notes](https://github.com/AgrDataSci/gosset/blob/bb5eb75c08fe3ab0f28931c093dcb66dea7c4185/NEWS.md)
