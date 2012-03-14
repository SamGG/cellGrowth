#' Plot of a growth curve showing raw data and fitted curve
#' 
#' @title Generic plot function for datatype cellGrowthFit
#' @param x growth curve object. See \code{\link{fitCellGrowth}}
#' @param scaleX scalar affecting the scaling of the x-axis. 
#' @param xlab plot parameter
#' @param ylab plot parameter
#' @param lwd plot parameter
#' @param ... optional plot parameters passed to the plot function
#' @author Andreas Neudecker
#' @examples 
#' # Parse file
#' dat = readYeastGrower( system.file("extdata", "Plate1_YPFruc.txt", package="cellGrowth") )
#' 
#' # fit
#' n <- names( dat$OD)[36]
#' fit <- fitCellGrowth(x=dat$time,z=log2(dat$OD[[n]]), model = "locfit")
#' plot(fit)
#' @export
plot.cellGrowthFit <- function (
		x, scaleX = 1, xlab="time", ylab="log2(OD)", lwd=0.5,...
){
	fit = x
	cols = c("#000000","#377EB8","#E41A1C")
	time <- attr(fit,"x")
	value <- attr(fit,"z")
	plot(time*scaleX,value,lwd=lwd, xlab=xlab, ylab=ylab,...)
	if( !inherits(fit, "try-error" ) ){
		lines(time*scaleX, predict(fit,time), lwd=2, col=cols[1])
	
		if ( !is.null(attr(fit,"pointOfMaxGrowthRate")) )
		{
			# max growth rate
			xMaxGrowthRate = time[attr(fit,"pointOfMaxGrowthRate")]
			yMaxGrowthRate = predict(fit,data.frame(x=xMaxGrowthRate))
			maxGrowthRate = attr(fit,"maxGrowthRate")
		
			xMaxGrowthRate = xMaxGrowthRate*scaleX
			maxGrowthRate = maxGrowthRate/scaleX
			abline(yMaxGrowthRate - maxGrowthRate*xMaxGrowthRate,maxGrowthRate,untf=TRUE,col=cols[2],lwd=2)
		}
		# maximum
		abline(h=attr(fit,"max"),col=cols[3],lwd=2)
		
		if ( is.null(attr(fit,"pointOfMaxGrowthRate")) )
			legend( "bottomright", legend=c( 
							paste("fit (",attr(fit,"model"),")"),
							paste("maximum (",format(attr(fit,"max"),scientific=TRUE,digits=4),")")
					),
					lwd=lwd, col=cols[-2] )
		else
			legend( "bottomright", legend=c( 
							paste("fit (",attr(fit,"model"),")"),
							paste("max growth rate (",format(maxGrowthRate,scientific=TRUE,digits=4),")"),
							paste("maximum (",format(attr(fit,"max"),scientific=TRUE,digits=4),")")
					),
					lwd=lwd, col=cols )

	}
}