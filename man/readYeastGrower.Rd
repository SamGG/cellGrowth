\name{readYeastGrower}
\alias{readYeastGrower}
\title{Read Yeast Grower data files}
\usage{
  readYeastGrower(file)
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
  Read raw data file from Yeast Grower software
}
\examples{
# Get file names
# Parse file
dat = readYeastGrower( system.file("extdata", "Plate1_YPFruc.txt", package="cellGrowth") )

# fit
n <- names( dat$OD)[36]
fit <- fitCellGrowth(x=dat$time,z=log2(dat$OD[[n]]), model = "locfit")
plot(fit)
}
\author{
  Julien Gagneur
}
\seealso{
  \code{\link{readGenios} }
}

