"createExternalParameters" <- function(
	subjects,           						#@ Subjects for which to create parameters
	file,               						#@ File name for data to import
	names,              						#@ Names of fixed effects in data
	range,                						#@ Range of acceptable values for parameters
	betNames,           						#@ Between subject effects variables in the data
	betNums,             						#@ Integer vector mapping between subject effects onto fixed effects
	errStruc = c("None", "Proportional", "Additive", "Log-Normal"),		#@ function to map effect (Additive, Proportional, Log-Normal or None)
	subset = NULL,         						#@ Subset to apply to the data
	refCol,         				     		#@ Reference column for sampling
	refColName,         						#@ Column name in data for referenced sampling
	idCol = getEctdColName("Subject"),        	#@ ID variable name for return data
	seed = .deriveFromMasterSeed(), 			#@ Random seed
	flagName = getEctdColName("ParOmit"),  		#@ Parameter omit flag name
	refColSuffix = "refCol",
	dataId = idCol,								#@ Subject column in the imported dataset
	workingPath = getwd() 						#@ Working directory
){
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# createExternalParameters.R 21/12/09
	#
	# Author: Romain    
	###############################################################################
	# DESCRIPTION: generate a set of model parameter values for each subject in a 
	#              simulated dataset
	# KEYWORDS: component:data:parameter io
	###############################################################################

	# Set the random number seed
	set.seed(seed) 

	# Check and parse inputs
	subjects <- .expandSubjects( subjects )                      
	names    <- parseCharInput( names , convertToNumeric = FALSE, checkdup = TRUE, valid = TRUE)  
	idCol    <- parseCharInput( idCol , convertToNumeric = FALSE, expected = 1, valid = TRUE)
	dataId   <- parseCharInput( dataId, convertToNumeric = FALSE, expected = 1, valid = TRUE)
	flagName <- parseCharInput( flagName, convertToNumeric = FALSE, expected = 1, valid = TRUE)
	allnames <- c( dataId, names )
	nSubjects <- get("nSubjects")

	# Check the error structure
	getErrStruc <- try(match.arg(errStruc))
	if (class(getErrStruc) == "try-error") {
		errStruc <- casefold(substring(errStruc, 1, 1), upper = TRUE)
		getErrStruc <- try(match.arg(errStruc))
		if (class(getErrStruc) == "try-error") ectdStop("Error Structure input should be 'None', 'Additive', 'Log-Normal' or 'Proportional'")
	}

	## handle the between names
  	if(!missing(betNames)) {
    	betNames <- parseCharInput( betNames, convertToNumeric = FALSE, valid = TRUE)
		if ( missing(betNums) && length(betNames) != length(names)) 
			ectdStop("No `betNums` and length of names different than length of betNames")
		betNums <- if(missing(betNums)) 1:length(betNames) else parseCharInput(betNums, checkdup = TRUE, convertToNumeric = TRUE)
		if( any(betNums<1 | betNums>length(names))) ectdStop("Incorrect value of `betNums`")
		if(length(betNums) != length(betNames)) ectdStop("`betNums` does not have the same length as `betNames`")
		allnames <- c( allnames, betNames )
	}
	if (any( duplicated(allnames) )) ectdStop(paste("Duplicated names", paste(allnames, collapse = ","), sep="\n\t"))

	# Read and parse the file
	idata <- .readAndCheckInputFile( file.path(workingPath, file), allnames  )
	
	# If reference column data is provided
	if (!missing(refColName)) {

		validNames(refColName)
    	# find the real refColName ( remove the suffix )
    	datanames <- names(idata)

		# verify that the refCol variable is in the file
    	rx <- paste("\\.", refColSuffix, "$", sep="")
    	if( any( regexpr(rx, refColName)  > 0 ) ) refColName <- refColName %-~% rx
    	suffixedRefColName <- paste(refColName, refColSuffix, sep=".")
    	if ( suffixedRefColName %in% datanames ) refColName <- refColName
		else {
			if(!(refColName %in% datanames)) ectdStop( "The reference column is not in the imported parameter file, checked:\n `$refColName`, `$suffixedRefColName`" )    
    	}

		# Reference column data supplied
		if(!missing(refCol) ){
   			refCol <- parseCharInput( refCol )
    		refcolvalues <- idata[[refColName]]
    		if( !all(refCol %in% refcolvalues) ) ectdStop( "Not all the values of `refCol` are found in the `$refColName` column of the imported data"  )
      		if( length(refCol) != length(subjects) ) ectdStop( "The number of elements in `refCol` is not the same as the number of subjects" )
    	} 
		else refCol <- unique(idata[[refColName]])
	}
	
	## Subset the input data 
	if( !is.null(subset)){
		subset <- parseRangeCode( subset )
		idata <- idata[ eval(subset, idata), , drop = FALSE ]
		if( nrow(idata) == 0 ) ectdStop( "No data left after applying the subset" )
	}  

  	## replace dataId by idCol
  	names( idata )[ names(idata) == dataId ] <- idCol
	if (!missing(refColName) && refColName == dataId) refColName <- idCol
  
  	## figure out how to subset the data to possibly take care of the refCol settings
  	## see design doc for parameter version 0.3 section 3.2.3.2
  	
	if (missing(refColName)){
		idx <- sample( nrow(idata) , size = nSubjects, replace = TRUE )
		idata <- idata[ idx, , drop = FALSE]
  	} 
	else {
    	# <SLOW>
    	# potential slow code: calling sample within sapply
		idx <- sapply( refCol, function(x) sample(which(idata[[refColName]] == x), size = 1 ))
		# </SLOW>
    	idata <- idata[ idx, , drop = FALSE]
		if ( refColName %in% names(idata) ) refColName <- paste(refColName, refColSuffix, sep=".")
		refData <- idata[setdiff(names(idata), idCol)]
		refData[[refColName]] <- idata[[idCol]]
		idata <- refData
	}
	
	# Apply error structure
	out <- switch( getErrStruc, 
		"Additive" = {                    # Additive: fixed + between 
			fixed   <- idata[, names, drop = FALSE]
			between <- idata[, betNames, drop=FALSE]
			fixed[, names[betNums]] <- fixed[, names[betNums]] + between
			fixed
		}, 
		"Log-Normal" = {                    # Log Normal: fixed * exp ( between )
			fixed   <- idata[, names, drop = FALSE]
			between <- idata[, betNames, drop=FALSE]
			fixed[, names[betNums]] <- fixed[, names[betNums]] * exp (  between ) 
			fixed
		}, 
		"Proportional" = {                    # Proportional: fixed * (1 + between)
			fixed   <- idata[,c(names),drop = FALSE]
			between <- idata[,betNames,drop=FALSE]
			fixed[, names[betNums]] <- fixed[, names[betNums]] * ( 1 + between ) 
			fixed
		}, 
		"None" = {                    # keep fixed and between
			idata[, c( names, if(missing(betNames)) NULL else betNames ) ]
		}
	)

	# Deal with "range" clause
	if (!missing(range)) out <- .checkParameterRange(out, range, flagName)
	else out[[flagName]] <- rep(0, nrow(out))

	# Build the return dataset
	outData <- data.frame( subjects, out, row.names = 1:nSubjects )
	names(outData) <- c(idCol, names(out))
	if (!missing(refColName) && any(names(idata) == refColName)) outData[[refColName]] <- idata[[refColName]]
	outData
}
