#' Guess initial parameters values for growth models
#' 
#' The relative.height.at.lag parameter should be close to the relative height of the point,
#' where the curve reaches its maximal slope. If the fitting fails, try to set this parameter
#' to a different value.
#' @title Guess growth models parameters
#' @param x \code{numeric} vector: time points
#' @param z code{numeric} vector: log(OD)
#' @param relative.height.at.lag \code{numeric} scalar (see Details)
#' @return  A list with entries: 
#'          \item{mu}{ \code{numeric} scalar: maximal growth rate parameter}
#'          \item{l}{ \code{numeric} scalar: time lag parameter}
#'          \item{z0}{\code{numeric} scalar: minimal log(OD) parameter }
#'          \item{zmax}{ \code{numeric} scalar: maximal log(OD) parameter } 
#' @author Julien Gagneur
#' @export
#' @examples
#' x <- 1:1000
#' z <- gompertz(x, mu=0.01, l=200, z0=1, zmax=5)+rnorm(length(x),mean=0,sd=0.25)
#' guess <- guessCellGrowthParams(x,z,relative.height.at.lag=0.5)
#' fit <- nls(z~gompertz(x,mu,l,z0,zmax),start=guess)
#' 	plot(x,z)
#' lines(x,predict(fit,x),lwd=2,col="red")
guessCellGrowthParams = function(x,z, relative.height.at.lag = 0.1){
	list(
			mu = max(diff(z)/diff(x)), 												
			l = min(x[ z > (max(z)-min(z))*relative.height.at.lag + min(z)]),		
			z0 = min(z),															
			zmax = max(z) 															
			)
}
