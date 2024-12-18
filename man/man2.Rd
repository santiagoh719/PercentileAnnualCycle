\name{MEP}
\alias{MEP}

\title{Moving window Empirical Percentile}
\description{
  Computes the moving window Empirical Percentile (MEP).
}
\usage{
  MEP(Nday_vec, PP_vec, Per, Window = 29)
}
\arguments{
  \item{Nday_vec}{Vector with the Julian Day associated with the PP_vec}

  \item{PP_vec}{Numeric Vector for which percentile will be estimated}

  \item{Per}{numeric, percentile to be computed}

  \item{Window}{integer, window size}
}

\value{
  Returns a data.frame with a percentile for each Julian Day.
}

\details{
  \code{MEP} Computes the moving window Empirical Percentile (MEP).
}
