\name{MEP}
\alias{MEP}

\title{Moving window Empirical Percentile}
\description{
  Compute de moving window Empirical Percentile (MEP).
}
\usage{
  MEP(Nday_vec, PP_vec, Per, Window = 29)
}
\arguments{
  \item{Nday_vec}{Vector with the Julian Day associated to the PP_vec}

  \item{PP_vec}{Numeric Vector for which percentile will be compute}

  \item{Per}{numeric, percentile to be computed}

  \item{Window}{integer, window size}
}

\value{
  Returns a data.frame with a percentile for each Julian Day.
}

\details{
  \code{MEP} Compute de moving window Empirical Percentile (MEP).
}
