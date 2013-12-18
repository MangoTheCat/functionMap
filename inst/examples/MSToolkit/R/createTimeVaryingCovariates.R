createTimeVaryingCovariates <- function(
		subjects,
		names,
		mean,
		covariance = 0,
		range = NULL,
		digits,
		maxDraws = 100, 
		seed = .deriveFromMasterSeed(),
		idCol = getEctdColName("Subject"), 
		timeCol = getEctdColName("Time"),
		treatPeriod
){
	set.seed( seed )

	## sanity checks on the inputs
	if( missing(mean) ) ectdStop("`mean` is needed in `createTimeVaryingCovariates`")
	subjects <- .expandSubjects( subjects )
	nSubjects <- get("nSubjects")

	# mean list
	if (is.list(mean)) {
		mean <- lapply(mean, parseCharInput)
		if (!all(sapply(mean, length) == length(mean[[1]]))) ectdStop("the time length of all the covariates should be the same")
	} else {
		mean  <- list(parseCharInput( mean  ))
	}

	# number of covariates and time points
	nCov <- length( mean )
	nTime <- length(mean[[1]])

	# check the length of 'treatPeriod' argument
	if (missing(treatPeriod)) ectdStop("`treatPeriod` is required when creating time-varying covariates")
	if (length(treatPeriod) != nTime) ectdStop("the length of `treatPeriod` must be equal to the number of time points")

	# check names
	names <- if(missing(names)) { 
		"X" %.% 1:nCov
	} else { 
		parseCharInput( names , checkdup = TRUE, convertToNumeric = FALSE)
	}

	validNames( names, idCol )

	if(length(names) != length(mean)) {
		ectdStop(
				"Dimension mismatch between `names` and `mean`"  %.nt% 
				"`mean`  of length: " %.% length(mean) %.nt%
        		"`names` of length: "%.% length(names) ) 
	}

	# covariance list
	if (is.list(covariance)) {
		covariance <- lapply(covariance, parseCovMatrix, nTime)
		if (length(covariance) != nCov) ectdStop("the length of covariance list should be equal to the number of covariates")
	} else {
		covariance <- parseCovMatrix( covariance, nTime )
		covariance <- rep(list(covariance), nCov )
		if (nCov > 1) warning("there is only 1 covariance matrix, use it for all the time point")
	}
	
	
	# maxDraws
	maxDraws <- as.integer(maxDraws)                      
	if( maxDraws < 1 ) ectdStop("The maximum number of draws should be a positive integer")

	# digits
	if( !missing(digits) && digits < 0) ectdStop("The `digits` argument must be positive")
   
	# range
	if( is.null(range) ) {
		range <- rep(list(NULL), nCov)
	} else if (is.list(range)) {
		range <- lapply(range, parseRangeCode)
		if (length(range) != nCov) ectdStop("the length of range list should be equal to the number of covariates")
	} else {
		ectdStop("The `range` argument must be list for every covariance")
	}
	
	OUT <- NULL
	for (i in 1:nCov) {
		rangei <- range[[i]]
		namei <- names[i]
		namev <- paste("T", treatPeriod, sep = ".")
		if (!is.null(rangei)) rangei <- sapply(1:nTime, FUN = function(X) gsub(namei, paste("T", treatPeriod[X], sep = "."), rangei, fixed = TRUE))
		
		tmp <- createContinuousCovariates(
				subjects = subjects,
				names = namev,
				mean = mean[[i]], 
				covariance = covariance[[i]], 
				range = rangei,
				digits = digits,
				maxDraws = maxDraws,
				seed = seed,
				idCol = idCol, 
				includeIDCol = TRUE)
		
		tmp <- reshape(tmp, idvar = idCol, varying = list(2:ncol(tmp)),
				timevar = timeCol, v.names = namei, times = treatPeriod, direction = "long")
		rownames(tmp) <- NULL
		
		if (is.null(OUT)) {
			OUT <- tmp
		} else {
			OUT <- merge(OUT, tmp)
		}
	}
	
	OUT <- OUT[ do.call("order", OUT[c(idCol, timeCol)]), , drop=FALSE]
	return(OUT)
  
}
  
