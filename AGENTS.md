# Instructions for AI agents

gosset is an R package for analysing data from experimental agriculture, mainly rankings from decentralized on-farm trials that use the tricot approach. It prepares rankings for the Plackett-Luce model (from the PlackettLuce package), and helps select, validate and present models. People share this repository with an assistant either to **analyse their data** with gosset or to **change the package**. Find out which before you start.

## Before you write any code

1. Check the installed version with `packageVersion("gosset")` and read the help of every function you use (`?rank_tricot`). Arguments change between versions, so do not rely on memory.
2. Look for an existing function before writing your own. gosset and PlackettLuce already cover most ranking tasks, and a hand-made parser or model is the most common source of errors.
3. Statistical models come from PlackettLuce. gosset prepares data and summarises fits. If a result looks wrong, check the PlackettLuce documentation before changing gosset.
4. For the methods behind these functions and the evidence on tricot, read the knowledge base at https://github.com/AgrDataSci/tricot-knowledge-base (start with its `AGENTS.md`).

## Which function for which task

| Task | Function | Notes |
|---|---|---|
| Rankings from ClimMob or other tricot data in **wide** format (columns for the items A, B, C and for the best and worst choice) | `rank_tricot()` | Use `group = TRUE` for models with covariates. `additional.rank` adds the comparison with a local check. This is what ClimMob-analysis uses. |
| Rankings from data in **long** format (one row per participant and item, with a rank) | `rank_tricot2()` | Warns and skips missing or duplicated records. Check its help for stricter validation options in your installed version. |
| Rankings from measured values, such as yield | `rank_numeric()` | Soft-deprecated for tricot data since 1.5.1. Read `?rank_numeric` for the current advice before using it. |
| Fill unranked items so a model can run | `pseudo_rank()` | Adds information that was not observed. Say so when reporting. |
| Fit a model of item worth | `PlackettLuce::PlackettLuce()` | Uses pseudo-rankings by default (`npseudo = 0.5`), which shrink estimates. Report the setting. |
| Explain differences in rankings by covariates (climate, gender, location) | `PlackettLuce::pltree()` | Then `node_labels()`, `node_rules()`, `top_items()` and `plot()` to describe the tree. |
| Compare models out of sample | `crossvalidation()` | Returns AIC, deviance, pseudo R-squared and, for trees, Kendall's tau per fold. |
| Test whether rankings differ between groups | `likelihood_ratio()` | Asymptotic chi-square test. Treat p-values from small or sparse groups with caution. |
| Select covariates | `btpermute()`, `akaike_weights()` | |
| Agreement between rankings or traits | `kendallTau()`, `kendallTau_bootstrap()`, `kendallTau_permute()`, `kendallW()`, `compare()` | |
| Summarise item performance for reports | `preference_index()`, `pairwise_probs()`, `reliability()` | `reliability()` compares each item with a check. |
| Risk across environments | `regret()` | Worst regret, a conservative criterion for choosing varieties. |
| Plot worth | `worth_map()`, `worth_bar()` | |
| Model fit | `pseudoR2()` | |

Example data: `nicabean` (long format bean trial), `breadwheat`, `cassava`, `kenyachoice`. The vignettes show complete analyses.

## Pitfalls

- **Tricot data are relative.** Rankings give the probability that one item beats another, not yield in kg/ha. Do not report worth as if it were a yield.
- **Zero means not ranked.** In a rankings matrix, 0 marks an item the participant did not evaluate.
- **Check connectivity.** Items must be linked through shared comparisons. Sparse or disconnected designs give unstable worth, which pseudo-rankings hide.
- **Ties and partial rankings are allowed** by PlackettLuce. Do not drop them without a reason.
- **Keep the item order.** Labels A, B and C are random per participant. Always map them to item names through the trial's design, never by position across participants.
- **Personal data.** Tricot data hold names, phone numbers and locations. Never put them in examples, issues or test files.

## Changing the package

- Use `=` for assignment, not `<-`. Use base R where possible.
- Document with roxygen and run `devtools::document()`. Never edit files in `man/` by hand.
- Every fix needs a test in `tests/testthat/` that fails before the fix and passes after it. Run `devtools::test()` and `devtools::check()`, and report what was actually run. Never claim a change works without running it.
- Keep changes backwards compatible, or record the break in `NEWS.md` with the reason. Changes that alter results (estimates, p-values, counts) always go in `NEWS.md`.
- ClimMob-analysis (https://github.com/AgrDataSci/ClimMob-analysis) depends on gosset, including the internal functions `build_tree_branches()` and `build_tree_nodes()`. Do not change these without checking that workflow.
- Work on a branch and open a pull request. Do not merge without review and passing checks.

## Where to find more

- Reference and vignettes: https://agrdatasci.github.io/gosset/ (when built with pkgdown 2.2.0 or later, the site also provides `llms.txt`)
- PlackettLuce: https://CRAN.R-project.org/package=PlackettLuce
- Issues: https://github.com/AgrDataSci/gosset/issues
