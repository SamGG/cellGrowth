#' Converts well ids to row and column
#' @title Convert well ids to row and column
#' @param wellId vector of well ids
#' @return vector of lists containing row and column
#' @author Andreas Neudecker
#' @export
#' @examples
#' getRowColumn(c("A01","B05"))
getRowColumn = function ( wellId )
{
	row=factor(substr(wellId,1,1))
	column=as.integer(substr(wellId,2,3))
	return(list(row=row,column=column))
}