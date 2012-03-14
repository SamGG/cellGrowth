#' Load a plate layout file and a file specifying the machine runs
#' 
#' See the provided example files for the layout and machine run file formats.
#' @title Create a well data frame
#' @param plateLayoutFile a file containing the plate layout.
#' The file must contain a column named \code{plate} and a column named \code{well}
#' @param machineRunFile a file containing the machine runs
#' The file must contain columns named \code{directory}, \code{filename} and \code{plate} specifying
#' the directory and filename of the data for the corresponding run. The column \code{use} is optional.
#' If present, only rows with \code{use == TRUE} are put into the dataframe. 
#' @return an object of class well and data.frame
#' @author Andreas Neudecker
#' @export
#' @examples 
#' plateLayout <-  system.file("extdata", "plateLayout.txt", package="cellGrowth")
#' machineRun <-  system.file("extdata", "machineRun.txt", package="cellGrowth")
#' well <- wellDataFrame(plateLayout,machineRun)
#' plot(well,plate=1)
wellDataFrame <- function ( plateLayoutFile, machineRunFile )
{
	machineRun <- read.delim(machineRunFile)
	if ( "use" %in% names(machineRun)){
		machineRun <- subset(machineRun , use=="TRUE")
		## refactor factors
		for(n in names(machineRun)){
			if(is.factor(machineRun[[n]]))
				machineRun[[n]] = factor(machineRun[[n]])
		}
	}
	## get directory relative to current directory and not to machineRunsFile
	machineRun$directory = file.path(dirname(machineRunFile), machineRun$directory)
	
	plateLayout <- read.delim(plateLayoutFile)
	
	m <- merge(plateLayout, machineRun, by.x="plate", by.y="plate")
	well <- as.data.frame(m)
	class(well) = c("well", class(well))
	return(well)
}
