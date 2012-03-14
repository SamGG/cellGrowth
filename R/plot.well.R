#' Plots well plate as lattice xyplot.
#' 
#' This function calls \code{\link{plotPlate}} for the plate \code{plate}.
#' The ... parameter is passed to the \code{\link{plotPlate}} function.
#' @title Generic plot function for datatype well
#' @param x the well object
#' @param file which plate file to plot? If NULL (default) the first file is taken.
#' @param labelColumn column in the well object to take label for the wells from
#' @param calibration \code{function} or \code{list} of \code{functions}. If calibration is a
#' function it is applied to all raw data. If it is a list, the well dataframe must contain a
#' column named \code{machine}. Depending on that column the according function in the list
#' is applied to the raw data.
#' @param ... optional plot parameters, see details
#' @export
#' @author Andreas Neudecker

plot.well = function(
		x,
		file=NULL,
		labelColumn=NULL,
		calibration=identity,
		...
){	
	well=x
	if(is.null(file)){
		file = file.path(well$directory, well$filename)[1]
	}

	if ( is.list(calibration))
	{
		machine = unique(well$machine[file.path(well$directory,well$filename)==file])

		if ( is.null( machine ) || is.null(calibration[[machine]]) )
		{
			if ( is.null( machine ) )
				warning(paste("Machine not found in dataframe for file",file,". No calibration is done."))
			if (is.null(calibration[[machine]]))
				warning(paste("No calibration function for the machine",machine,"in file",file,". No calibration is done."))
			
			calibration = identity
		}
		else
			calibration = calibration[[machine]]
		
	}
	
	## labels
	if ( is.null(labelColumn))
		labels = NULL
	else
	{
		wh = file.path(well$directory, well$filename)==file
		labels = well[[labelColumn]][wh]
		names(labels) = as.character(well[["well"]])[wh]
	}
	
	plotPlate(
			file=file, 
			calibration=calibration,
			labels=labels,
			...)
}