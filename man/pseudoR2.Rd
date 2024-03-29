% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AAA-pseudoR2.R
\name{pseudoR2}
\alias{pseudoR2}
\alias{pseudoR2.default}
\alias{pseudoR2.pltree}
\alias{pseudoR2.bttree}
\title{Pseudo R-squared}
\usage{
pseudoR2(object, ...)

\method{pseudoR2}{default}(object, ...)

\method{pseudoR2}{pltree}(object, newdata = NULL, ...)

\method{pseudoR2}{bttree}(object, ...)
}
\arguments{
\item{object}{a model object of class glm, gnm, lm, pltree or bttree}

\item{...}{additional arguments affecting the R-squared produced}

\item{newdata}{a data.set with testing data}
}
\value{
A data frame containing the pseudo R-squared coefficients:
\item{logLik}{log-likelihood}
\item{logLikNull}{Null log-likelihood}
\item{MaxLik}{maximum likelihood pseudo R-squared}
\item{CraggUhler}{Cragg and Uhler's pseudo R-squared}
\item{McFadden}{McFadden pseudo R-squared}
}
\description{
Regression coefficient to evaluate goodness-of-fit in a given model when 
ordinary least squares (OLS) are not available. The algorithm computes estimates 
from the maximum likelihood through an iterative process. These estimates are called 
'pseudo R-squared' because they look like 'R-squared' in the sense that they are on 
a similar scale (from 0 to 1), with higher values indicating better model fit.
}
\examples{

data("airquality")

mod = glm(Temp ~ Wind + Solar.R,
           data = airquality,
           family = poisson())

pseudoR2(mod)

}
\references{
Agresti A. (2002). Categorical Data Analysis. John Wiley & Sons, Inc., 
Hoboken, NJ, USA. doi:10.1002/0471249688

Hunter D. R. (2004). The Annals of Statistics, 32(1), 384–406. 
http://www.jstor.org/stable/3448514

Cragg, J. G., & Uhler, R. S. (1970). The Canadian Journal of 
Economics 3(3), 386-406. doi:10.2307/133656

McFadden, D. (1973). Conditional logit analysis of qualitative choice behavior.
}
\seealso{
Other goodness-of-fit functions: 
\code{\link{kendallTau}()},
\code{\link{kendallW}()}
}
\author{
Kauê de Sousa and Jacob van Etten
}
\concept{goodness-of-fit functions}
