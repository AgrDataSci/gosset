% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crossvalidation.R, R/deviance.R
\name{crossvalidation}
\alias{crossvalidation}
\alias{AIC.bttree}
\alias{deviance.bttree}
\alias{deviance.pltree}
\alias{AIC.gnm}
\alias{deviance.gnm}
\title{Cross-validation}
\usage{
crossvalidation(formula, data, k = 10, folds = NULL, seed = NULL, ...)

\method{AIC}{bttree}(object, newdata = NULL, ...)

\method{deviance}{bttree}(object, newdata = NULL, ...)

\method{deviance}{pltree}(object, newdata = NULL, ...)

\method{AIC}{gnm}(object, newdata = NULL, ...)

\method{deviance}{gnm}(object, newdata = NULL, ...)
}
\arguments{
\item{formula}{an object of class "formula" (or one that can be 
coerced to that class): a symbolic description of the model to be fitted,
of the form \eqn{y ~ x1 + ... + xn}}

\item{data}{a data frame (or object coercible by as.data.frame to a data frame)
containing the variables in the model}

\item{k}{an integer for the number of bins in the cross-validation}

\item{folds}{an optional vector or list of vectors specifying the \var{k}-folds 
in the cross-validation}

\item{seed}{integer, the seed for random number generation. If \code{NULL} (the default), 
\pkg{gosset} will set the seed randomly}

\item{...}{additional arguments passed the methods of the chosen model}

\item{object}{a model object}

\item{newdata}{a data.frame with test data}
}
\value{
an object of class \code{gosset_cv} with the cross-validation 
goodness-of-fit estimates, which are:
\item{AIC}{Akaike Information Criterion}
\item{deviance}{Model deviance}
\item{logLik}{Log-Likelihood}
\item{MaxLik}{Maximum likelihood pseudo R-squared}
\item{CraggUhler}{Cragg and Uhler's pseudo R-squared}
\item{McFadden}{McFadden pseudo R-squared}
\item{kendallTau}{the Kendall correlation coefficient}
}
\description{
Methods for measuring the performance of a predictive model on sets of 
test data in Bradley-Terry model from \pkg{psychotree}, Generalized Linear and 
Generalized Nonlinear models from \pkg{gnm}, and Plackett-Luce model from 
\pkg{PlackettLuce}
}
\examples{
\donttest{
# Generalized Linear Models
if (require("gnm")) {
data("airquality")

cv = crossvalidation(Temp ~ Wind + Solar.R,
                      data = airquality,
                      k = 3,
                      seed = 999,
                      family = poisson())
}                    
# Plackett-Luce Model
if(require("PlackettLuce")) {
# beans data from PlackettLuce
data("beans", package = "PlackettLuce")

G = rank_tricot(data = beans,
                 items = c(1:3),
                 input = c(4:5),
                 additional.rank = beans[c(6:8)],
                 group = TRUE)

beans = cbind(G, beans)

# take seasons as bins
k = length(unique(beans$season))
folds = as.integer(as.factor(beans$season))

cv = crossvalidation(G ~ maxTN,
                      data = beans,
                      k = k,
                      folds = folds,
                      minsize = 100)
}
}
                
}
\references{
Elder J. F. (2003). Journal of Computational and Graphical Statistics, 
12(4), 853–864. \doi{https://doi.org/10.1198/1061860032733}

James G., et al. (2013). \doi{https://doi.org/10.1007/978-1-4614-7138-7}

Whitlock M. C. (2005). Journal of Evolutionary Biology, 18(5), 1368–1373. 
\doi{https://doi.org/10.1111/j.1420-9101.2005.00917.x}
}
\seealso{
\code{\link[psychotree]{bttree}}, 
\code{\link[gnm]{gnm}},
\code{\link[PlackettLuce]{pltree}}

Other model selection functions: 
\code{\link{btpermute}()}
}
\author{
Kauê de Sousa, Jacob van Etten and David Brown
}
\concept{model selection functions}
