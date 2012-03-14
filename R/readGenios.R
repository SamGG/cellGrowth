#' Read raw data file form Tecan Genios instrument
#' 
#' @title Read Tecan Genios data files
#' @param file filename
#' @return a list with entries: 
#'         \item{time }{ a \code{numeric} vector of time points}
#'         \item{OD }{a \code{data.frame} vector of measured OD. The colnames are the well names.}  
#'         \item{read }{a \code{numeric} vector of read numbers}
#'         \item{temperature }{a \code{numeric} vector of temperatures}
#'         \item{header}{a \code{character} vector: the header of the file }  
#' @author Julien Gagneur
#' @seealso \code{\link{readYeastGrower}}
#' @export
#' @examples # Get file names
#' # Parse file
#' dat = readGenios( system.file("extdata", "tecan_genios.txt", package="cellGrowth") )
#' 
#' # fit
#' n <- names( dat$OD)[36]
#' fit <- fitCellGrowth(x=dat$time,z=log2(dat$OD[[n]]), model = "locfit",locfit.h=6*60*60)
#' plot(fit)

readGenios = function(file){
	## parse header
	i=1
	hdr = readLines(file,n=i)
	while(substr(hdr[length(hdr)], 1, 7)!="Rawdata"){
		i=i+1
		hdr = readLines(file,n=i)
	}
	raw = read.delim(file, header= TRUE, skip=i)
	well <- colnames(raw)[4:ncol(raw)]
	OD = raw[,4:ncol(raw)]
	list(time = raw$Time, OD = OD, read = raw$Cycle.No., temperature=raw$Temp., header=hdr )
}
