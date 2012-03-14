\name{rosso}
\alias{rosso}
\title{Rosso growth model}
\usage{
  rosso(x, mu, l, z0, zmax)
}
\arguments{
  \item{x}{vector: time points for which log(OD) must be
  computed}

  \item{mu}{scalar: maximal growth rate parameter}

  \item{l}{scalar: time lag parameter}

  \item{z0}{scalar: minimal log(OD) parameter}

  \item{zmax}{scalar: maximal log(OD) parameter}
}
\value{
  vector: log(OD) for the time points given in \code{x}
}
\description{
  Rosso growth model
}
\details{
  Rosso model is z0 if x<=l zmax - log( 1 + (exp(zmax-z0)
  -1)*exp(-mu*(x-l) ) ) otherwise
}
\examples{
x = 1:1000
y = rosso(x, mu=0.01, l=200, z0=1, zmax=5)
plot(x,y)
}
\author{
  Julien Gagneur
}

