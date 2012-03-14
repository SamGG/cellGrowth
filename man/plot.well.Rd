\name{plot.well}
\alias{plot.well}
\title{Generic plot function for datatype well}
\usage{
  plot.well(x, file = NULL, labelColumn = NULL,
    calibration = identity, ...)
}
\arguments{
  \item{x}{the well object}

  \item{file}{which plate file to plot? If NULL (default)
  the first file is taken.}

  \item{labelColumn}{column in the well object to take
  label for the wells from}

  \item{calibration}{\code{function} or \code{list} of
  \code{functions}. If calibration is a function it is
  applied to all raw data. If it is a list, the well
  dataframe must contain a column named \code{machine}.
  Depending on that column the according function in the
  list is applied to the raw data.}

  \item{...}{optional plot parameters, see details}
}
\description{
  Plots well plate as lattice xyplot.
}
\details{
  This function calls \code{\link{plotPlate}} for the plate
  \code{plate}. The ... parameter is passed to the
  \code{\link{plotPlate}} function.
}
\author{
  Andreas Neudecker
}

