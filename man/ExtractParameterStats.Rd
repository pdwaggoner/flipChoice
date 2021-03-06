% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractparameterstats.R
\name{ExtractParameterStats}
\alias{ExtractParameterStats}
\title{Extract Parameter Statistics from a Choice Model Fit}
\usage{
ExtractParameterStats(fit)
}
\arguments{
\item{fit}{An object of class \code{"FitChoice"} produced by a call
to \code{FitChoiceModel} with \code{algorithm = "HB-Stan"}.}
}
\value{
In the case of LCA, a matrix of parameter estimates, standard
    errors, t-statistics and p-values. For HB, a matrix containing a summary
    of the parameter samples from the MCMC results stored in \code{fit},
    including mean, standard error, effective sample size, and rhat for each
    mean and standard deviation parameter.
}
\description{
If the Choice Model is from a Latent Class Analysis, a matrix of parameter
estimates, standard errors, t-statistics and p-values is returned.
}
\details{
If the Choice Model is from Hierarchical Bayes, this function produces a
matrix sample statistics of the mean and standard deviation parameters for
the distribution from which individual coefficients are sampled.
}
\seealso{
\code{\link[rstan]{monitor}}, \url{https://www.displayr.com/convergence-hb-maxdiff/},
    \code{\link{FitChoiceModel}} for an example, \code{\link{TracePlots}},
\code{\link{PlotPosteriorIntervals}}

Other HB diagnostics: \code{\link{PlotPosteriorIntervals}},
  \code{\link{TracePlots}}
}
\concept{HB diagnostics}
