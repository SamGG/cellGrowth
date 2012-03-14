#' Read raw data file from Yeast Grower software 
#' 
#' @title Read Yeast Grower data files
#' @param file filename
#' @return a list with entries: 
#'         \item{time }{ a \code{numeric} vector of time points}
#'         \item{OD }{a \code{data.frame} vector of measured OD. The colnames are the well names.}  
#'         \item{read }{a \code{numeric} vector of read numbers}
#'         \item{temperature }{a \code{numeric} vector of temperatures}
#'         \item{header}{a \code{character} vector: the header of the file }  
#' @author Julien Gagneur
#' @seealso \code{\link{readGenios} }
#' @export
#' @examples # Get file names
#' # Parse file
#' dat = readYeastGrower( system.file("extdata", "Plate1_YPFruc.txt", package="cellGrowth") )
#' 
#' # fit
#' n <- names( dat$OD)[36]
#' fit <- fitCellGrowth(x=dat$time,z=log2(dat$OD[[n]]), model = "locfit")
#' plot(fit)

readYeastGrower = function(file){
	## parse header
	i=1
	hdr = readLines(file,n=i)
	while(substr(hdr[length(hdr)], 1, 7)!="[Data:]"){
		i=i+1
		hdr = readLines(file,n=i)
	}
	raw = read.delim(file, header= TRUE, skip=i)
	well <- colnames(raw)[4:ncol(raw)]
	OD = raw[,4:ncol(raw)]
	read = as.numeric(unlist(lapply(strsplit(as.character(raw$Read.DateTime), "="), function(x) x[1])))
	list(time = raw$RunT.s., OD = OD, cycle.no = read, temperature=raw$Temp, header=hdr )
}
