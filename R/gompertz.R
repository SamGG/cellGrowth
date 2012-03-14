#' Gompertz growth model as defined in Zwietering et al.
#' 
#' @title Gompertz growth model
#' @param x \code{numeric} vector: time points for which log(OD) must be computed
#' @param mu \code{numeric} scalar: maximal growth rate parameter
#' @param l \code{numeric} scalar: time lag parameter
#' @param z0 \code{numeric} scalar: minimal log(OD) parameter
#' @param zmax \code{numeric} scalar: maximal log(OD) parameter
#' @return \code{numeric} vector: log(OD) for the time points given in \code{x}
#' @author Julien Gagneur
#' @export
#' @references Zwietering, et al. Modeling of the Bacterial Growth Curve, APPLIED AND ENVIRONMENTAL MICROBIOLOGY, 1990.
#' @examples x = 1:1000
#' y = gompertz(x, mu=0.01, l=200, z0=1, zmax=5)
#' plot(x,y)

gompertz = function(x, mu, l, z0, zmax){
	z0 + (zmax -z0)*exp( - exp(mu*exp(1)*(l-x)/(zmax-z0) + 1 ) )
}
