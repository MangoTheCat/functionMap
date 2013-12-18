"analyzeRep" <- function(
		analysisCode		,           #@	File containing the actual analysis code to run on the data
		replicate		,               #@	Replicate number of data to analyze
		removeMissing	= TRUE	,     	#@	Logical flag: remove rows where the "Missing" Flag is set to 1?
		removeParOmit	= TRUE	,     	#@	Logical flag: remove rows where the "Parameter Omit" Flag is set to 1?
		removeRespOmit= TRUE	,     	#@	Logical flag: remove rows where the "Reponse Omit" Flag is set to 1?
		interimCode	= NULL,             #@	Interim analysis Code to run on the data between interims (eg. can be used to drop doses)
		software = c("R", "SAS")	,   	#@	Software system in which the analysis should take place: R or SAS
		seed =	.deriveFromMasterSeed(),#@	Random number generation seed
		parOmitFlag	 = getEctdColName("ParOmit"),   #@	Parameter omit flag name
		respOmitFlag = getEctdColName("RespOmit"),  #@	Response omit flag name
		missingFlag = getEctdColName("Missing"),   #@	Missing flag name
		interimCol	 = getEctdColName("Interim"),   #@	Interim variable name
		doseCol	     = getEctdColName("Dose"), 	    #@	Dose variable name
		initialDoses = NULL,						#@ Initial set of doses to use in "interim 1"
		stayDropped = TRUE,				#@ Dose dropping flag: if a dose is dropped, should it stay dropped?
		fullAnalysis = TRUE,			#@ Perform a full analysis
		workingPath = getwd(),
		method = getEctdDataMethod()
){ 
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# analyzeRep.R Wed Jul 04 12:20:41 BST 2007 @514 /Internet Time/
	#
	# Author: Romain    
	###############################################################################
	# DESCRIPTION: wrapper for the analysis step
	# KEYWORDS: component:analysis
	###############################################################################
	
	# Inner function: Retain values
	"innerRetainValues" <- function(vec) {
		if (all(is.na(vec))) return(rep(0, length(vec)))
		rleNa <- rle(is.na(vec))
		if (rleNa$values[1]) vec[1:(rleNa$lengths[1])] <- 0
		isMiss <- is.na(vec)
		approx(which(!isMiss), vec [ !isMiss ], 1:length(vec), "constant", rule = 2)$y
	}
	
	# Check replicate number input
	if( !is.numeric(replicate) || length(replicate) != 1 || replicate <= 0 )  
		ectdStop("replicate must be a single positive integer")
	
	## check that the software is SAS or R 
	software <- try( match.arg(software), silent = TRUE )
	if (class(software) == "try-error") ectdStop("The software should be `R` or `SAS`")
	
	# checking the macro code
	if (software == "R") {
		if (class(analysisCode) == "function") analysisCode <- .checkFun( analysisCode, "data" )
		else {
			if (!file.exists(file.path(workingPath, analysisCode))) ectdStop(paste("Cannot find R analysis script file \"", analysisCode, "\"", sep=""))
		}
	}
	else {
		if (!file.exists(file.path(workingPath, analysisCode))) ectdStop(paste("Cannot find SAS analysis script file \"", analysisCode, "\"", sep=""))
	}
	
	## checks on inputs
	doseCol      <- parseCharInput( doseCol     , expected = 1, valid = TRUE, convertToNumeric = FALSE )

	## import the data
	idata <- readData( dataNumber = replicate, dataType = "Replicate", 
			variables = doseCol, workingPath = workingPath, method = method)
	columns <- names( idata )
	doses <- sort( unique( idata[[ doseCol ]]  ) )
	
	## check the flags  
	parOmitFlag	 <- parseCharInput( parOmitFlag	, expected = 1, convertToNumeric = FALSE )
	if (removeParOmit) {
		valid <- try( validNames( parOmitFlag ), silent = TRUE )
		if ( class(valid) == "try-error" ) ectdStop("Invalid format for parameter omit flag variable name")
		if ( !(parOmitFlag %in% columns) ) removeParOmit <- FALSE
	}

	respOmitFlag <- parseCharInput( respOmitFlag, expected = 1, convertToNumeric = FALSE )
	if (removeRespOmit) {
		valid <- try( validNames( respOmitFlag ), silent = TRUE )                    
		if ( class(valid) == "try-error" ) ectdStop("Invalid format for response omit flag variable name")
		if ( !(respOmitFlag %in% columns) ) removeRespOmit <- FALSE
	}
	
	missingFlag <- parseCharInput( missingFlag, expected = 1, convertToNumeric = FALSE )
	if (removeMissing) {	
		valid <- try( validNames( missingFlag ), silent = TRUE )
		if ( class(valid) == "try-error" ) ectdStop("Invalid format for 'Missing' flag variable name")
		if ( !(missingFlag %in% columns) ) removeMissing <- FALSE
	}
	
	interimCol   <- parseCharInput( interimCol  , expected = 1, convertToNumeric = FALSE, valid = TRUE )
	valid <- try( validNames( interimCol ), silent = TRUE )
	if ( class(valid) == "try-error" ) ectdStop("Invalid format for interim allocation variable name")
	if (!(interimCol %in% columns) || !is.numeric(idata[[interimCol]]) || any(idata[[interimCol]] < 0) ) interimCode <- NULL
	
	## check the software
	software <- try( match.arg( software ), silent= TRUE)
	if (class(software) == "try-error") ectdStop("Software should be `R` or `SAS`")
	
	## subset data according to the remove Flags
	removeSub <- NULL                
	if( removeParOmit ) removeSub <- c( removeSub , paste( "( ", parOmitFlag  , " != 1 ) ", sep = "") )
	if( removeMissing ) removeSub <- c( removeSub , paste( "( ", missingFlag , " != 1 ) ", sep = "") ) 
	if( removeRespOmit) removeSub <- c( removeSub , paste( "( ", respOmitFlag , " != 1 ) ", sep = "") )
	if (length(removeSub)) {
		removeSub <- paste( removeSub , collapse = " & " )
		idata <- idata[ eval( parse( text = removeSub ), idata ), ,drop = FALSE ]
	}

	# Perform full analysis of the data including all the doses found in the data (if required)
	if (fullAnalysis | is.null(interimCode)) {
		.log(" ... full analysis")
		fullOutput <-  performAnalysis( analysisCode = analysisCode, seed = seed, 
				data = idata, software = software, doses = doses, doseCol = doseCol, 
				workingPath = workingPath  )                                                                    

		## add more variables to the dataset
		if (is.data.frame(fullOutput) && (nRows <- nrow(fullOutput))) {
			alldata <- data.frame( 
				INTERIM = rep(0, nRows), 
				INTERIMC = rep("FULL", nRows), 
				fullOutput, 
				stringsAsFactors = FALSE)
			if (doseCol %in% names(fullOutput)) {
				alldata$INCLUDED = rep(1, nRows) 
				alldata$DROPPED = rep(0, nRows)
			}
			alldata$STOPPED <- rep(0, nRows)			
		}
		else alldata <- NULL
	}
	else alldata <- NULL

	## cycle through the interims
	if ( !is.null(interimCode) ){

		# check if there is code
		uniqueInterim <- unique( idata [[interimCol]] )
		if (missing(interimCode)) ectdStop("No interim Code found")    

		# check if the function exists
		interimCode <- try( match.fun(interimCode), silent =TRUE )
		if (class(interimCode) == "try-error") ectdStop("Cannot find the interimCode function")
		
		# number of interim
		nInterim <- max( idata[[ interimCol ]])

		includeRows <- dropped <- beenDropped <- NULL
		includeDoses <- if (!is.null(initialDoses)) initialDoses else doses

		for( int in 1:nInterim ){
			
			.log( " ... interim $int / $nInterim" )

			# make the new subset
			includeRows <- rbind(includeRows, cbind(int, includeDoses))

			# perform the analysis on the interim data
			newAnalysis <- try( 
					performAnalysis( analysisCode, seed = seed, data = idata, 
							software = software, includeRows = includeRows, 
							doses = doses, doseCol = doseCol,
							interimCol = interimCol, workingPath = workingPath ), 
					silent = TRUE )
			if (class(newAnalysis) == "try-error") ectdStop("Error when executing `performAnalysis`\n\t$newAnalysis")

			# If anything has come back from the analysis, behave accordingly
			if (is.data.frame(newAnalysis) && (nRows <- nrow(newAnalysis))) {

				# Add interim columns to the data
				newAnalysis <- data.frame(
					INTERIM = rep(int, nRows),  
					INTERIMC = if(int == nInterim) rep("FINAL", nRows) else rep(int, nRows), 
					newAnalysis, 
					stringsAsFactors = FALSE)
				if (doseCol %in% names(newAnalysis)) newAnalysis$INCLUDED <- as.numeric(newAnalysis[[doseCol]] %in% includeDoses)

				# Call the interimAnalysis function to get data changes
				iList <- try(interimAnalysis( newAnalysis, interimCode, uniqueDoses = doses ))
				if (class(iList) == "try-error") ectdStop("Interim analysis step failed")
				
				# Work out actions based on the return "DROP" element
				dropKeep <- any(c("DROP", "KEEP") %in% names(iList))
				if ( dropKeep ) {
					if ("DROP" %in% names(iList)) {
						# Specifies dose levels to drop as a numeric vector
						dropped <- intersect(iList$DROP, includeDoses)
						if (length(dropped)) includeDoses <- setdiff(includeDoses, dropped)
					}
					else {
						# Logical vector, corresponding to unique doses
						if (length(iList$KEEP) != length(doses)) ectdStop("Logical 'drop doses' return from interim analysis not of correct length")
						whichInclude <- doses[iList$KEEP]
						dropped <- setdiff(includeDoses, whichInclude)
						includeDoses <- whichInclude
					}

					# If dose has been dropped before, don't allow it to be reopened
					if (stayDropped) {
						beenDropped <- union(beenDropped, dropped)
						includeDoses <- setdiff(includeDoses, beenDropped)
					}
				
				} 
				else dropped <- NULL
				
				# Add columns to the analysis output
				if (doseCol %in% names(newAnalysis)) newAnalysis$DROPPED <- as.numeric(newAnalysis[[doseCol]] %in% dropped)
				newAnalysis$STOPPED = rep(1 * ( "STOP" %in% names(iList) && iList$STOP ), nrow(newAnalysis))

				# Add new analysis to existing data
				alldata <- rbind( alldata, newAnalysis)

				# Do we need to stop the trial? 
				if ( "STOP" %in% names(iList) && iList$STOP ) break			# Stop the trial if specified
				if (!length(includeDoses)) break							# Stop the trial if no doses to include in next interim
			}
		}
	}

	if (any(myTest <- names(alldata) == "INTERIM")) names(alldata)[myTest] <- interimCol
	alldata 
	
}

