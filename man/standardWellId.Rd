\name{standardWellId}
\alias{standardWellId}
\title{Make standard names for well ids}
\usage{
  standardWellId(wellId)
}
\arguments{
  \item{wellId}{vector of well ids}
}
\value{
  standard well name
}
\description{
  Make standard names for well in 96 well plates
}
\details{
  A1 -> A01 A01 -> A01
}
\examples{
standardWellId( c("A1", "B01", "H2"))
}
\author{
  Julien Gagneur
}

