---
title: 'gosset: An R package for analysis and synthesis of ranking data in agricultural experimentation'
abstract: null
output:
  pdf_document: default
  word_document: default
editor_options:
  markdown:
    wrap: 72
bibliography: paper.bib
header-includes: \usepackage{caption} \captionsetup[figure]{labelformat=empty}
---

>Kauê de Sousa^1,2[*]^, Jacob van Etten^2^, David Brown^3,4^, Jonathan Steinke^2,5^  
>-
>
^1^ Department of Agricultural Sciences, Inland Norway University of Applied Sciences, 2318 Hamar, Norway  
^2^ Digital Inclusion, Bioversity International, Parc Scientifique Agropolis II, 34397, Montpellier Cedex 5, France   
^3^ Laboratory of Geo-Information Science and Remote Sensing, Wageningen University & Research, Droevendaalsesteeg 3, 6708 PB, Wageningen, The Netherlands   
^4^ Digital Inclusion, Bioversity International, 30501, Turrialba, Costa Rica   
^5^ Humboldt University Berlin, Berlin, Germany  

^[*]^ Correspondence should be addressed to: <kaue.desousa@inn.no>

# Abstract
Appropriate data management and analysis are necessary to produce practical information from agricultural experimentation data. There is also an ongoing trend advocating for programmatic tools that supports reproducible workflows in scientific research. We developed the R package gosset, providing functionality to support analysis workflows with rank-based models, such as Plackett-Luce and Bradley-Terry. The gosset package facilitates data preparation, modelling and results presentation stages. We demonstrate the functionality of the package with a case of on-farm evaluations of common bean (*Phaseolus vulgaris* L.) genotypes in Nicaragua.

::: frontmatter
::: keyword
data-driven agriculture, Plackett-Luce model
:::
:::

\newpage

# Required Metadata {#required-metadata .unnumbered}

# Current code version {#current-code-version .unnumbered}

| **Nr.** | **Code metadata description**                                   | **Please fill in this column**                             |
|:-----------------|:--------------------------|:-------------------------|
| C1      | Current code version                                            | 0.4.003                                                    |
| C2      | Permanent link to code/repository used for this code version    | <https://github.com/AgrDataSci/gosset>                     |
| C3      | Code Ocean compute capsule                                      |                                                            |
| C4      | Legal Code License                                              | MIT                                                        |
| C5      | Code versioning system used                                     | Git                                                        |
| C6      | Software code languages, tools, and services used               | R                                                          |
| C7      | Compilation requirements, operating environments & dependencies |                                                            |
| C8      | If available Link to developer documentation/manual             | <https://agrdatasci.github.io/gosset/>                     |
| C9      | Support email for questions                                     | [kaue.desousa@inn.no](mailto:kaue.desousa@inn.no){.email} |

: *Code metadata (mandatory)*

# Motivation and significance

*Introduce the scientific background and the motivation for developing
the software.*

*Explain why the software is important, and describe the exact
(scientific) problem(s) it solves.*

*Indicate in what way the software has contributed (or how it will
contribute in the future) to the process of scientific discovery; if
available, this is to be supported by citing a research paper using the
software.*

*Provide a description of the experimental setting (how does the user
use the software?).*

*Introduce related work in literature (cite or list algorithms used,
other software etc.).*

Participatory experimentation approaches have been increasingly applied in agricultural
research [@deRoo2019farm]. While collecting data in ranking format is uncommon in general
agricultural research settings, it is often collected in participatory experiments [@Coe2002AnalyzingRA].
Recently developed approaches for on-farm experimentation,
such as the tricot methodology are based on the collection of data in ranking format [@vanetten_beza_2019].
On the other hand, newly proposed approaches for synthesis of crop variety evaluation data 
largely depend on the analysis of ranking data [@brown2020data].
The analysis of ranking data requires the use of appropriate statistical models such as Plackett-Luce  [@luce_individual_1959;@Plackett] or Bradley-Terry [@bradley1952rank]. Functionality for fitting Bradley-Terry and 
Plackett-Luce models are available in R with the packages BradleyTerry2 and PlackettLuce
respectively [@BradleyTerry2; @Turner2020]. However, extended functionality was required for the entire 
data science workflow, which usually includes: (1) Data preparation and cleaning, 
(2) modelling and validation, and (3) results presentation. For (1) gosset provides functions for 
converting and preparing data into ranking or pairwise format required by the packages PlackettLuce and BradleyTerry2
respectively. For (2), gosset provides functions for model selection and validation using cross-validation. In the 
case of (3), enhanced functionality for plotting model results is provided by the gosset package.

# Software description

*Describe the software in as much as is necessary to establish a
vocabulary needed to explain its impact.*

The R package gosset provides functionality supporting the analysis
workflows in agricultural experimentation, especially with rank-based
approaches. The package is available in The Comprehensive R Archive Network (CRAN)
and can be installed by executing ``install.packages("gosset")``.

## Software Architecture

*Give a short overview of the overall software architecture; provide a
pictorial component overview or similar (if possible). If necessary
provide implementation details.*

The R package gosset is structured following the guidelines described in the manual
for creating R add-on packages [@r_extensions_2022]. This structure basically consist of 
files DESCRTIPTION, LICENSE, NAMESPACE and NEWS, and directories data, dev, docs, inst,
man, R, and vignettes. The package functions were developed following the S3 methods
style [@r_extensions_2022] and are contained in the R sub-directory.

## Software Functionalities

*Present the major functionalities of the software.*

**Data management and preparation**

When data from agricultural experiments is not in ranking format, it
should be transformed to be used as inputs into R packages for the
analysis of ranking data. For instance, the Plackett-Luce model [@luce_individual_1959;@Plackett]
is implemented in the R package as PlackettLuce,
which requires the data to be formatted as ranking matrix. Another
example is the Bradley-Terry model [@bradley1952rank], implemented
in the package BradleyTerry2 [@BradleyTerry2] and which requires the
input data to be formatted as paired comparisons. For these cases,
gosset provides the functions ``rank_numeric`` and ``rank_binomial`` respectively. The
function rank_numeric transforms a set of numeric values into an ordinal
ranking, considering if higher numeric values should be ranked first or
not. The function ``rank_binomial`` transforms data in rankings format into
pairwise comparisons, as required by the package BradleyTerry2.
Additionally, gosset provides the function ``rank_tricot``, for the case
when the experimental data is collected from tricot trials. In those trials,
farmers rank the technology under evaluation (e.g., crop variety) stating which
is the best and which is the worst from a set of three [@vanetten_beza_2019].

**Modelling**

The gosset package provides the following functions to support model
selection and validation of Bradley-Terry and Plackett-Luce models. The function ``pseudoR2``
computes goodness-of-fit measure McFadden's pseudo-R2 [@mcfadden1973conditional]. The ``AIC``
function computes the Akaike Information Criterion[@akaike1974]. The ``kendallTau`` function
computes the Kendall-tau rank correlation coefficient [@kendall_1938]. 



**Visualization and results presentation**  

``plot``  

``worth_map``  

``worth_map``  


``regret``  

``reliability``


# Illustrative Examples
We demonstrate the functionality of the gosset package using the 
trial dataset "nicabean", which consists of trial data collected in
Nicaragua following the tricot approach. We use the Plackett-Luce model
implemented in the R package *PlackettLuce*[@Turner2020]. We use climate data
as model covariates to investigate the effect fo climate factors on the performance 
of common bean (*Phaseolus vulgaris* L.) genotypes. For obtaining the climate data,
we use the *nasapower* package[@nasapower]. Climatic indices were computed with the
*climatrends* package[@climatrendspkg].

First, the required packages and data are loaded.
``` r
library("gosset")
library("PlackettLuce")
library("climatrends")
library("nasapower")

data("nicabean", package = "gosset")

dat <- nicabean$trial

covar <- nicabean$covar

traits <- unique(dat$trait)

```
Make a PlackettLuce rank using the function ``rank_numeric``
```r
R <- vector(mode = "list", length = length(traits))

for (i in seq_along(traits)) {
  
  dat_i <- subset(dat, dat$trait == traits[i])
  
  R[[i]] <- rank_numeric(data = dat_i,
                         items = "item",
                         input = "rank", 
                         id = "id", 
                         ascending = TRUE)
}

```
Kendall correlation between traits using kendallTau()
```

```

Worth map 
```

```

Plackett-Luce tree using environmental data 
```

```


Model selection using crossvalidation
```

```

pseudoR2

```r

```

``regret``
```r

```
 
 ``reliability``


# Impact

***This is the main section of the article and the reviewers weight the
description here appropriately***

*Indicate in what way new research questions can be pursued as a result
of the software (if any).*

*Indicate in what way, and to what extent, the pursuit of existing
research questions is improved (if so).*

*Indicate in what way the software has changed the daily practice of its
users (if so).*

*Indicate how widespread the use of the software is within and outside
the intended user group.*

*Indicate in what way the software is used in commercial settings and/or
how it led to the creation of spin-off companies (if so).*


Reproducible and efficient workflows are fundamental in scientific research (Lowndes et al. 2017).
The gosset package provides functionality that was not previously available from 
other R packages and which enabled scientific studies based on the analysis of
ranking data. This functionality enables making the entire workflow to be reproducible
and more efficient. The utility of the gosset package has been demonstrated by enabling 
studies based on the analysis of ranking data. For instance, both @vanEtten2019 and  @deSousa2021
applied the Plackett-Luce model in combination with recursive partitioning [@Turner2020;@Zeileis2008].
In both studies, the gosset package supported the data preparation, model validation and results presentation
tasks. The gosset package is freely available and can be downloaded from 
CRAN https://cran.r-project.org/package=gosset.


# Conclusions

*Set out the conclusion of this original software publication.*

We described the functionality of the R package gosset to support 
the synthesis and analysis of ranking data. The package provide functions not available in
existing R packages for analyzing ranking data. We provided an illustrative example 
covering the main functionality across the stages involved in the analysis workflow.


# Conflict of Interest

No conflict of interest exists: We wish to confirm that there are no
known conflicts of interest associated with this publication and there
has been no significant financial support for this work that could have
influenced its outcome.



# Acknowledgements {#acknowledgements .unnumbered}

We acknowledge Olga Spellman (Science Writing Service of the Alliance of
Bioversity International and CIAT) for English editing of this
manuscript

# References

**Zenodo repository for the gosset package:** <https://doi.org/10.5281/zenodo.6339989>

::: thebibliography
:::

*Please add the reference to the software repository if DOI for software
is available.*  



