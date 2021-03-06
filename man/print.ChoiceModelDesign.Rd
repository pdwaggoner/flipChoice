% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.ChoiceModelDesign.R
\name{print.ChoiceModelDesign}
\alias{print.ChoiceModelDesign}
\title{Display choice model design output as an \code{htmlwidget}}
\usage{
\method{print}{ChoiceModelDesign}(x, css = NULL, nsmall = 2,
  digits = 2, ...)
}
\arguments{
\item{x}{An object of class \code{ChoiceModelDesign} output from
\code{\link{ChoiceModelDesign}}.}

\item{css}{Optional string giving the path to a file containing additional
CSS to style the htmlwidget output.}

\item{nsmall}{Integer; see \code{\link{format}}.}

\item{digits}{Integer; see \code{\link{format}}.}

\item{...}{Currently, ignored.}
}
\value{
An \code{htmlwidget} containing diagnostic information for
    the experimental design, including D-error, standard errors,
    frequenices, pairwise frequencies, the labeled design, and
    prior information. An attribute called \code{"ChartData"} also
    contains the labeled design.
}
\description{
Creates a \code{htmlwidget} summary of diagnostic information for a
choice model design output from \code{\link{ChoiceModelDesign}}.
}
\seealso{
\code{\link{ChoiceModelDesign}},
    \code{\link[rhtmlMetro]{Box}}
}
