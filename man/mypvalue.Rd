% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mypvalue.R
\name{mypvalue}
\alias{mypvalue}
\title{mypvalue Function}
\usage{
mypvalue(t0, xmax = 4, n = 20, alpha = 0.05)
}
\arguments{
\item{t0}{The observed t-statistic, that is tcalc}

\item{xmax}{The maximum value of x on the plot}

\item{n}{The sample size of the data}

\item{alpha}{The significance level for the test, which is alpha}
}
\value{
A plot of the t-distribution curve and a list containing the critical t-value of the test (q) and the calculated p-value.
}
\description{
Calculates the p-value for the given observed t-statistic and plots the t-distribution curve.
}
\examples{
\dontrun{mypvalue(tcalc, n = 30, alpha = 0.05)}
}
