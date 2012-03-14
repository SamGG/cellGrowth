\name{wellDataFrame}
\alias{wellDataFrame}
\title{Create a well data frame}
\usage{
  wellDataFrame(plateLayoutFile, machineRunFile)
}
\arguments{
  \item{plateLayoutFile}{a file containing the plate
  layout. The file must contain a column named \code{plate}
  and a column named \code{well}}

  \item{machineRunFile}{a file containing the machine runs
  The file must contain columns named \code{directory},
  \code{filename} and \code{plate} specifying the directory
  and filename of the data for the corresponding run. The
  column \code{use} is optional. If present, only rows with
  \code{use == TRUE} are put into the dataframe.}
}
\value{
  an object of class well and data.frame
}
\description{
  Load a plate layout file and a file specifying the
  machine runs
}
\details{
  See the provided example files for the layout and machine
  run file formats.
}
\examples{
plateLayout <-  system.file("extdata", "plateLayout.txt", package="cellGrowth")
machineRun <-  system.file("extdata", "machineRun.txt", package="cellGrowth")
well <- wellDataFrame(plateLayout,machineRun)
plot(well,plate=1)
}
\author{
  Andreas Neudecker
}

