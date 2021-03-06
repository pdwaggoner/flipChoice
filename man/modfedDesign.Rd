% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modfeddesign.R
\name{modfedDesign}
\alias{modfedDesign}
\title{Construct an Efficient Choice Model Design}
\usage{
modfedDesign(levels.per.attribute = NULL, prior = NULL,
  alternatives.per.question, n.questions, labeled.alternatives = FALSE,
  dummy.coding = TRUE, seed = 1776, n.sim = 10)
}
\arguments{
\item{levels.per.attribute}{A \emph{named} vector containing the
number of levels for each attribute with names giving the
attribute names.  See the Details.}

\item{prior}{Number vector or two-column matrix containing prior
values for the model coefficients.  The vector length or number
of rows in the matrix must correspond to the number of
attributes/attribute levels specified in
\code{levels.per.attribute}.  If \code{NULL}, the prior for the
coefficients is assumed to be identically zero.  In the matrix
case, the first column is taken as the prior mean for the
coefficients and the second is taken to be the prior variances.
If only one column is present, the prior for the coefficients
is assumed to be centered at those values.}

\item{alternatives.per.question}{Numeric value indicating the
number of profiles to show per question.}

\item{n.questions}{Numeric value specifying the total number of
questions/tasks to be performed by each respondent.}

\item{labeled.alternatives}{Logical; whether the first attribute
labels the alternatives.  See the Details.}

\item{dummy.coding}{Logical value indicating whether dummy coding
should be used for the attributes in the design matrix.  If
\code{FALSE}, effects coding is used.}

\item{seed}{Integer value specifying the random seed to use for the
algorithm.}

\item{n.sim}{Number of simulations to draw from the prior to initial
the algorithm.}
}
\value{
A list containing the following components.  \itemize{
    \item \code{design} - A numeric matrix wich contains an
    efficient design.  \item \code{error} - Numeric value
    indicating the D(B)-error of the design.  \item
    \code{inf.error} - Numeric value indicating the percentage of
    samples for which the D-error was \code{Inf}.
    \item\code{prob.diff} - Numeric value indicating the difference
    between the alternative with the highest and the one with the
    lowest probability for each choice set. If prior means and
    variances were supplied in \code{prior}, this is based on the
    average over all draws.  }
}
\description{
Uses the Modified Federov algorithm and incorporates prior beliefs
on the true coefficient values to find a design for a discrete
choice experiment that minimize D-error.
}
\details{
If \code{labeled.alternatives} is \code{TRUE}, an
    alternative-specific constant will be added to the model and
    the first component of \code{levels.per.attribute} is assumed
    to contain a name for the labels and the number of labels;
    i.e. the number of alternatives per question, and hence,
    \code{alternatives.per.question} will be ignored.
}
\examples{
levels.per.attribute <- c(car = 3, house = 3, boat = 3)
prior <- matrix(as.character(1), sum(levels.per.attribute - 1), 2)

## 3^3/3/9 design
\dontrun{
efficientDesign(levels.per.attribute, prior, 3, 9)
}
}
\references{
Huber, J. and K. Zwerina (1996). The Importance of
    Utility Balance in Efficient Choice Designs. Journal of
    Marketing Research.
\url{https://faculty.fuqua.duke.edu/~jch8/bio/Papers/Huber\%20Zwerina\%201996\%20Marketing\%20Research.pdf}

Zwerina, K., J. Huber and W. F. Kuhfeld. (2000). A General Method
for Constructing Efficient Choice
Designs. \url{http://support.sas.com/techsup/technote/mr2010e.pdf}
}
\seealso{
\code{\link[idefix]{Modfed}}
}
