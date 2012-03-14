#' Rosso growth model
#' 
#' Rosso model is 
#' z0 if x<=l
#' zmax - log( 1 + (exp(zmax-z0) -1)*exp(-mu*(x-l) ) ) otherwise 
#' 
#' @title Rosso growth model
#' @param x vector: time points for which log(OD) must be computed
#' @param mu scalar: maximal growth rate parameter
#' @param l scalar: time lag parameter
#' @param z0 scalar: minimal log(OD) parameter
#' @param zmax scalar: maximal log(OD) parameter
#' @return vector: log(OD) for the time points given in \code{x}
#' @author Julien Gagneur
#' @export
#' @examples x = 1:1000
#' y = rosso(x, mu=0.01, l=200, z0=1, zmax=5)
#' plot(x,y)

rosso = function(x, mu, l, z0, zmax){
	ifelse(
			x<=l,
			z0,
			zmax - log( 1 + (exp(zmax-z0) -1)*exp(-mu*(x-l) ) ) 
			)	
}



