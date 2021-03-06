#' Perform cross-validation to detect optimal bandwidth
#' 
#' This function needs a few minutes time. The "optimal" bandwidth is the largest bandwidth
#' which is in 95\% (cutoff parameter) of all cases within one standard deviation of the
#' best bandwidth. This should make the derivative of the fitted curve more robust. The raw values
#' from the machine might not be directly optical densities (OD),
#' which is needed to infer doubling time. Calibration functions for each machine can be
#' provided to map raw values into OD using the \code{calibration} parameter.
#' 
#' @title Bandwidth cross-validation
#' @param well well dataframe. See \code{\link{wellDataFrame}}.
#' @param fileParser Converts the file generated by the machine to
#' proper R format. See \code{\link{readYeastGrower}} for details.
#' @param getWellIds function or vector. If function its parameter is
#' the return value of fileParser. It should return a vector containing
#' the well ids (e.g. A01, A02, ...). You can set the well ids vector directly.
#' See \code{\link{getWellIdsTecan}}.
#' @param bandwidths vector of bandwidths to test on
#' @param nFold \code{integer}. In how many parts is the sample divided for cross-validation?
#' @param nWell \code{integer}. How many wells out of the well dataframe will be used
#' for cross validation?
#' @param cutoff \code{scalar} between 0 and 1. See details.
#' @param calibration \code{function} or \code{list} of \code{functions}. If function, calibration
#' is applied to all raw data. If list, the well dataframe must contain a column \code{machine}.
#' Depending on that column the according function in the list is applied to the raw data. See details
#' @param scaleY \code{function} applied to the calibrated data.
#' @return list with entries 
#' \item{bandwidth}{"optimal" bandwidth}
#' \item{well}{well dataframe}
#' \item{bandwidths}{tested bandwidths}
#' \item{err2}{squared error}
#' \item{err2std}{Standard deviation of squared error}
#' \item{muStd}{Standard deviation of max growth rate}
#' \item{oneStdOfMini}{bandwidths within one std of best}
#' @export 
#' @examples
#' folder <- system.file("extdata", package="cellGrowth")
#' well <- wellDataFrame(file.path(folder, "plateLayout.txt"), file.path(folder,"machineRun.txt"))
#' 
#' ## for a fast example, we use nWell = 1 here. Use a large number (default 100) for real life applications 
#' bw <- bandwidthCV(well, nWell=1)
#' @author Julien Gagneur and Andreas Neudecker
bandwidthCV = function( 
		well, 
		fileParser=readYeastGrower,
		getWellIds=getWellIdsTecan,
		bandwidths=seq(0.5*3600,10*3600, length.out=30),
		nFold=10,
		nWell=100,
		cutoff=0.95,
		calibration=identity,
		scaleY=log2
	)
{
	# function to split sample in n folds
	cv.folds = function (n ){
		split(sample(1:n), rep(1:nFold, length = n))
	}
	
	# function to predict max growth rate
	cvpred_mu = function(cv, h, x, z){
		ls = lapply(
				cv,
				function(fo){
					fit = tryCatch(fitCellGrowth( x = x[-fo], z = z[-fo], model = "locfit", h), warning=function(w){NA}, error=function(e){NA})
					if( !is.null(attr(fit,"maxGrowthRate")) ){
						rv = list(pred=predict( fit, x[fo]), mu = attr(fit, "maxGrowthRate"))
					}else{
						rv = list(pred=rep(NA, length(fo)), mu = NA)
					}
					rv
				}
		)
		list( pred=unlist(lapply(ls, function(x) x$pred)), mu  = unlist(lapply(ls, function(x) x$mu)))
	}
	
	# function to calculate error and std
	err2_mustd_well = function(f, w, hs){
		wh = which(file.path(well$directory, well$filename)==f & well$well == w)
		if(length(wh)>1)
			stop("2 well annotations for same well", well[wh,])
		
		dat = fileParser(f)
		if ( is.function ( getWellIds ))
			wellIds = getWellIds( dat )
		else
			wellIds = getWellIds
		
		x = dat$time
		
		## calibration
		if ( !is.function(calibration) && !is.list(calibration))
		{
			warning("Calibration function no function and no list of functions. No calibration is done.")
			y = dat$OD[[ match(w, wellIds)]]
		}
		if ( is.function(calibration))
			y = sapply(dat$OD[[ match(w, wellIds) ]],calibration)
		if ( is.list(calibration))
		{
			machine = well$machine[wh]
			if ( is.null( machine ) || is.null(calibration[[machine]]) )
			{
				if ( is.null( machine ) )
					warning(paste("Machine not found in dataframe for file",f,", well",w,". No calibration is done."))
				if (is.null(calibration[[machine]]))
					warning(paste("No calibration function for the machine",machine,"in file",f,", well",w,". No calibration is done."))
				y = dat$OD[[ match(w, wellIds)]]
			}
			else
				y = sapply(dat$OD[[ match(w, wellIds) ]],calibration[[machine]])
			
		}
		## negative ODs?
		if ( !all(y>=0))
		{
			warning("Negative ODs found. If you are using a calibration function, this might be the problem. Values are set to NAs")
			y[y<0] = NA
		}
		
		z = scaleY( y )
		
		cv = cv.folds(length(x))
		pms = lapply(hs, function(h) cvpred_mu(cv,h,x,z))
		pred = sapply(pms, function(x) x$pred)
		mu = sapply(pms, function(x) x$mu)
		
		err = pred - z[unlist(cv)]
		list( err2 = colMeans(err^2), err2std = apply(err^2, 2, sd), mustd =  apply(mu, 2, sd) )
	}
	
	## do cv
	ws = sample(nrow(well),nWell)				# wells to perform cv on
	hs = bandwidths								# possible bandwidths
	

	# calculate mu error and std
	err2_mustd = lapply(
			ws,
			function(w){
				cat(paste("Treating well",well$well[w],"in plate",well$file[w],"\n"))
				data <- err2_mustd_well(file.path(well$directory[w], well$file[w]), as.character(well$well[w]), hs)
			}
	)

	
	err2 = sapply(err2_mustd, function(x) x$err2)
	err2std = sapply(err2_mustd, function(x) x$err2std)
	mustd = sapply(err2_mustd, function(x) x$mustd)
	
	## which bandwidth are at one std of mini in average
	onestd_of_mini = sapply(
			1:ncol(err2),
			function(i){
				m = which.min(err2[,i])
				err2[,i] <= err2[m,i] + err2std[m,i]
			} 
	)
	
	## onestd_of_mini returns a list
	if(is.list(onestd_of_mini)){
		onestd_of_mini = do.call(cbind,onestd_of_mini)
	}
	
	# calculate "optimal" bandwidth
	bandwidth = hs[max( which(rowMeans(onestd_of_mini,na.rm=TRUE)>= cutoff))]
	
	return(
			list(
					bandwidth=bandwidth,
					well=well,
					bandwidths=hs, 
					err2=err2,
					err2std=err2std,
					muStd=mustd,
					oneStdOfMini=onestd_of_mini
		)
	)
}