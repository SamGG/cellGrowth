\name{plot.cellGrowthFit}
\alias{plot.cellGrowthFit}
\title{Generic plot function for datatype cellGrowthFit}
\usage{
  plot.cellGrowthFit(x, scaleX = 1, xlab = "time",
    ylab = "log2(OD)", lwd = 0.5, ...)
}
\arguments{
  \item{x}{growth curve object. See
  \code{\link{fitCellGrowth}}}

  \item{scaleX}{scalar affecting the scaling of the
  x-axis.}

  \item{xlab}{plot parameter}

  \item{ylab}{plot parameter}

  \item{lwd}{plot parameter}

  \item{...}{optional plot parameters passed to the plot
  function}
}
\description{
  Plot of a growth curve showing raw data and fitted curve
}
\examples{
# Parse file
dat = readYeastGrower( system.file("extdata", "Plate1_YPFruc.txt", package="cellGrowth") )

# fit
n <- names( dat$OD)[36]
fit <- fitCellGrowth(x=dat$time,z=log2(dat$OD[[n]]), model = "locfit")
plot(fit)
}
\author{
  Andreas Neudecker
}

