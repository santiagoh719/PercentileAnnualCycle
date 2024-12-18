\name{GAM_AnnualCycle}
\alias{GAM_AnnualCycle}

\title{GAM smoothing}
\description{
  Compute de smooth annual cycle with GAM.
}
\usage{
  GAM_AnnualCycle(vec, N_basicfun =9, fam ='gaussian')
}
\arguments{
  \item{vec}{Numeric Vector for which the smooth will be computed, it must have a length of 365 -one value for each Julian day-}

  \item{N_basicfun}{numeric, the dimension of the basis used to represent the smoothed term, see s from mgcv}

  \item{fam}{character, see family attribute of mgcv package}
}

\value{
  Returns a data.frame with the smooth value each Julian Day.
}

\details{
  \code{GAM_AnnualCycle}   Compute the smoothed annual cycle with GAM.

}
