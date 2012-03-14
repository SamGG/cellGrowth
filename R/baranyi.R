#' gamma function as defined in Kelly et al, 1999
#' 
#' @param z
gamm = function(z){
	ifelse(
			z<1,
			atan( sqrt(2)*z/(1-z^2) ),
			ifelse(
					z==1,
					pi/2,
					atan( sqrt(2)*z/(1-z^2) ) + pi
					)
			)	
}

#' B4 function as defined in Kelly et al, 1999
#' 
#' B4 is empirically correct as showed by the following test code:
#' z = seq(0,10,0.01)
#' plot(z[-1],diff(B4(z)))
#' lines(z[-1],0.01/(1+z[-1]**4), col="red")
#' 
#' @param z
B4 = function(z){
	( 1/(2*sqrt(2)) )*(
				0.5*log( (z^2+sqrt(2)*z+1)/(z^2-sqrt(2)*z+1) ) +
				gamm(z)
				)
}

#' A4 function as defined in Kelly et al, 1999
#' 
#' @param x
#' @param l
A4 = function(x, l){
	l*(x/l - B4(x/l))
}


#' Baranyi growth model as defined in Kelly et al.
#' 
#' @title Baranyi growth model
#' @param x \code{numeric} vector: time points for which log(OD) must be computed
#' @param mu \code{numeric} scalar: maximal growth rate parameter
#' @param l \code{numeric} scalar: time lag parameter
#' @param z0 \code{numeric} scalar: minimal log(OD) parameter
#' @param zmax \code{numeric} scalar: maximal log(OD) parameter
#' @return \code{numeric} vector: log(OD) for the time points given in \code{x}
#' @author Julien Gagneur
#' @references Kelly et al., The use of dummy data points when fitting bacterial growth curves,
#'             IMA Journal of Mathematics Applied in Medicine and Biology (1999) 16, 155-170
#' @export 
#' @examples x = 1:1000
#' y = baranyi(x, mu=0.01, l=200, z0=1, zmax=5)
#' plot(x,y)

baranyi = function(x, mu, l, z0, zmax){
	z0 + mu*A4(x,l) - log( 1+ (exp(mu*A4(x,l)) -1)/exp(zmax-z0) )	
}


