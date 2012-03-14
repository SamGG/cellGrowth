\name{baranyi}
\alias{baranyi}
\title{Baranyi growth model}
\usage{
  baranyi(x, mu, l, z0, zmax)
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
  Baranyi growth model as defined in Kelly et al.
}
\examples{
x = 1:1000
y = baranyi(x, mu=0.01, l=200, z0=1, zmax=5)
plot(x,y)
}
\author{
  Julien Gagneur
}
\references{
  Kelly et al., The use of dummy data points when fitting
  bacterial growth curves, IMA Journal of Mathematics
  Applied in Medicine and Biology (1999) 16, 155-170
}

