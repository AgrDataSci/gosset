<!-- badges: start -->
[![CRAN](https://www.r-pkg.org/badges/version/gosset)](https://cran.r-project.org/package=gosset)
[![CRAN checks](https://badges.cranchecks.info/worst/gosset.svg)](https://cran.r-project.org/web/checks/check_results_gosset.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/gosset)](https://cran.r-project.org/package=gosset)
[![DOI](https://zenodo.org/badge/173807877.svg)](https://zenodo.org/badge/latestdoi/173807877)
[![R-CMD-check](https://github.com/AgrDataSci/gosset/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AgrDataSci/gosset/actions/workflows/R-CMD-check.yaml)
[![ProjStatus](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/AgrDataSci/gosset/blob/master/LICENSE.md)
<!-- badges: end -->

# *gosset*: Tools for Data Analysis in Experimental Agriculture <img align="right" src="man/figures/logo.png">

## Overview

**gosset** analyses data from experimental agriculture, from data synthesis to model selection and visualisation. Its focus is ranking data from decentralised on-farm trials that use the tricot approach (triadic comparison of technology options), where each farmer ranks a small set of varieties or practices.

gosset turns raw trial data into rankings for the Plackett-Luce model of the [PlackettLuce](https://CRAN.R-project.org/package=PlackettLuce) package. It then helps you select, validate and present those models, and summarise item performance for variety release and reports. The package is named after W.S. Gosset aka 'Student', a pioneer of modern statistics in small sample experimental design and analysis.

## What's new in 2.0.0

Version 2.0.0 is the first CRAN release since 1.5.1, and it includes the statistical review released on GitHub as 1.6.0. Function names and arguments are unchanged, but some results change.

* **Corrected results.** `kendallTau()`, `crossvalidation()`, `preference_index()`, `regret()`, `likelihood_ratio()` and `kendallW()` now return correct values. `kendallTau()` p-values are two-sided by default, and `kendallW()` ignores items a participant did not rank.
* **Tree plots fixed.** `plot()` on a Plackett-Luce tree works again under current ggplot2, without ggparty and without warnings.
* **Lighter install.** gosset imports 9 packages instead of 14. ggparty, DescTools, tidyr and lifecycle are gone, and BradleyTerry2 and ggrepel are optional.

Read [NEWS](NEWS.md) before comparing with analyses run on earlier versions.

## Installation

The stable version is available through CRAN.

```r
install.packages("gosset")
```

The development version can be installed from GitHub.

```r
# install.packages("remotes")
remotes::install_github("AgrDataSci/gosset")
```

Two functions use optional packages. `btpermute()` needs BradleyTerry2, and `compare()` places labels with ggrepel when it is installed.

```r
install.packages(c("BradleyTerry2", "ggrepel"))
```

## Quick start

This example ranks common bean varieties from a tricot trial in Nicaragua, fits a Plackett-Luce tree with climate covariates and plots it.

```r
library("gosset")
library("PlackettLuce")

data("beans", package = "PlackettLuce")

# rankings from the best and worst choices, plus the comparison
# with the local variety
G = rank_tricot(data = beans,
                items = c(1:3),
                input = c(4:5),
                group = TRUE,
                additional.rank = beans[c(6:8)])

dat = cbind(G, beans[, c("maxTN", "season", "lon")])

# Plackett-Luce tree, rankings split by covariates
tree = pltree(G ~ maxTN + season + lon, data = dat)

node_rules(tree)

top_items(tree, top = 3)

plot(tree)
```

## Main functions

| Task | Function |
|---|---|
| Rankings from tricot data in wide format (ClimMob) | `rank_tricot()` |
| Rankings from data in long format | `rank_tricot2()` |
| Fill unranked items so a model can run | `pseudo_rank()` |
| Describe a Plackett-Luce tree | `node_labels()`, `node_rules()`, `top_items()`, `plot()` |
| Compare models out of sample | `crossvalidation()` |
| Test whether rankings differ between groups | `likelihood_ratio()` |
| Select covariates | `btpermute()`, `akaike_weights()` |
| Agreement between rankings or traits | `kendallTau()`, `kendallW()`, `compare()` |
| Item performance for reports | `preference_index()`, `pairwise_probs()`, `reliability()`, `regret()` |
| Plot worth | `worth_map()`, `worth_bar()` |
| Model fit | `pseudoR2()` |

Models come from PlackettLuce. gosset prepares the data and summarises the fits.

## Learn more

The vignettes show complete analyses.

* [Case study 1: common bean on-farm testing](https://agrdatasci.github.io/gosset/articles/vignette-1-trait-prioritization-and-crop-performance.html)
* [Case study 2: consumer evaluation of cassava](https://agrdatasci.github.io/gosset/articles/vignette-2-gari-eba-consumer-testing.html)

The full reference is at <https://agrdatasci.github.io/gosset/>. If you work with an AI assistant, point it to [AGENTS.md](AGENTS.md), a short guide to choosing functions and avoiding common pitfalls.

## Citation

Get citation information in R with `citation(package = "gosset")`.

> de Sousa K, Brown D, Steinke J, van Etten J (2023). gosset: An R package for analysis and synthesis of ranking data in agricultural experimentation. *SoftwareX* 22:101402. <https://doi.org/10.1016/j.softx.2023.101402>

## Meta

* Please [report any issues or bugs](https://github.com/AgrDataSci/gosset/issues).
* License: MIT.
* You are welcome to contribute to *gosset*. Please read the [contribution guidelines](CONTRIBUTING.md).
* The *gosset* project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in it you agree to abide by its terms.
