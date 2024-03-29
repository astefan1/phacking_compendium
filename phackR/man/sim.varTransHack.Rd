% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variableTransformation.R
\name{sim.varTransHack}
\alias{sim.varTransHack}
\title{Simulate p-hacking with variable transformations
Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations}
\usage{
sim.varTransHack(
  nobs,
  transvar,
  testnorm = FALSE,
  strategy = "firstsig",
  alpha = 0.05,
  iter = 1000,
  shinyEnv = FALSE
)
}
\arguments{
\item{nobs}{Integer giving number of observations}

\item{transvar}{Which variables should be transformed? Either "x" (for x variable), "y" (for y variable), or "xy" (for both)}

\item{testnorm}{Should variables only be transformed after a significant test for normality of residuals?}

\item{strategy}{String value: One out of "firstsig", "smallest", "smallest.sig"}

\item{alpha}{Significance level of the t-test (default: 0.05)}

\item{iter}{Number of simulation iterations}

\item{shinyEnv}{Is the function run in a Shiny session? TRUE/FALSE}
}
\description{
Simulate p-hacking with variable transformations
Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
}
