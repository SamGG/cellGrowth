\name{getRowColumn}
\alias{getRowColumn}
\title{Convert well ids to row and column}
\usage{
  getRowColumn(wellId)
}
\arguments{
  \item{wellId}{vector of well ids}
}
\value{
  vector of lists containing row and column
}
\description{
  Converts well ids to row and column
}
\examples{
getRowColumn(c("A01","B05"))
}
\author{
  Andreas Neudecker
}

