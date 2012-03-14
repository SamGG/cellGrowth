\name{gompertz}
\alias{gompertz}
\title{Gompertz growth model}
\usage{
  gompertz(x, mu, l, z0, zmax)
}
\arguments{
  \item{x}{\code{numeric} vector: time points for which
  log(OD) must be computed}

  \item{mu}{\code{numeric} scalar: maximal growth rate
  parameter}

  \item{l}{\code{numeric} scalar: time lag parameter}

  \item{z0}{\code{numeric} scalar: minimal log(OD)
  parameter}

  \item{zmax}{\code{numeric} scalar: maximal log(OD)
  parameter}
}
\value{
  \code{numeric} vector: log(OD) for the time points given
  in \code{x}
}
\description{
  Gompertz growth model as defined in Zwietering et al.
}
\examples{
x = 1:1000
y = gompertz(x, mu=0.01, l=200, z0=1, zmax=5)
plot(x,y)
}
\author{
  Julien Gagneur
}
\references{
  Zwietering, et al. Modeling of the Bacterial Growth
  Curve, APPLIED AND ENVIRONMENTAL MICROBIOLOGY, 1990.
}

