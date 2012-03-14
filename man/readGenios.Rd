\name{readGenios}
\alias{readGenios}
\title{Read Tecan Genios data files}
\usage{
  readGenios(file)
}
\arguments{
  \item{file}{filename}
}
\value{
  a list with entries: \item{time }{ a \code{numeric}
  vector of time points} \item{OD }{a \code{data.frame}
  vector of measured OD. The colnames are the well names.}
  \item{read }{a \code{numeric} vector of read numbers}
  \item{temperature }{a \code{numeric} vector of
  temperatures} \item{header}{a \code{character} vector:
  the header of the file }
}
\description{
  Read raw data file form Tecan Genios instrument
}
\examples{
# Get file names
# Parse file
dat = readGenios( system.file("extdata", "tecan_genios.txt", package="cellGrowth") )

# fit
n <- names( dat$OD)[36]
fit <- fitCellGrowth(x=dat$time,z=log2(dat$OD[[n]]), model = "locfit",locfit.h=6*60*60)
plot(fit)
}
\author{
  Julien Gagneur
}
\seealso{
  \code{\link{readYeastGrower}}
}

