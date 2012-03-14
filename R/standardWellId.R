#' Make standard names for well in 96 well plates
#' 
#' A1 -> A01
#' A01 -> A01
#' @title Make standard names for well ids
#' @param wellId vector of well ids
#' @return standard well name
#' @author Julien Gagneur
#' @export
#' @examples standardWellId( c("A1", "B01", "H2"))
standardWellId = function( wellId ){
    if(!is.character(wellId)) wellId=as.character(wellId)
    row = substr(wellId, 1, 1)
    col = as.integer(substr(wellId, 2, nchar(wellId)))
    paste(row, sprintf("%02d", col) , sep="")
}

