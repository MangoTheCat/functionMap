"applyPredCode" <- function(
		df, 										#@ Dataset within which to apply $PRED statements
		pred, 										#@ Parsed $PRED statements
		respCol = getEctdColName("Response"),		#@ Response column name
		report = TRUE,								#@ Produce textual report
		keepCols = respCol,							#@ Columns to keep in the return data
		verbose = getEctdVerbose()					#@ Verbose report of data progression
){

	# Check data
	if (!is.data.frame(df)) ectdStop("Input must be a data frame")
	outDf <- df <- df [ setdiff(names(df), respCol) ]
	
	# Pad out text reporting
	ncMax <- max(nchar(pred)) + 1
	addSpace <- sapply(ncMax - nchar(pred), function(N) paste(rep(" ", N), sep="", collapse=""))	
	padPred <- paste(pred, addSpace)
	
	# Loop around the statements
	if (report) cat(paste("\n # Attempting to create Response variable '", respCol, "'\n", sep=""))
	
	nPred <- length(pred)
	for (i in 1:nPred) {
		if (report) cat(paste("\n > ", padPred[i], " :", sep=""))
		tryCommand <- try(within(outDf, eval(parse(text = pred[i]))), silent = TRUE)
		if (class(tryCommand) == "try-error") {
			if (report) {
				cat("FAILED")
				errorMessage <- substring(tryCommand[1], regexpr(":", tryCommand[1])+1 )
				cat(" (", gsub("\n", "", errorMessage), " )", sep="")
			}
		}
		else {
			if (report) cat("PASSED")
			outDf <- tryCommand
		}
		if (verbose) {
			cat("\n"); print(head(outDf)); cat("\n")
		}
	}
	
	# Check whether we have created a response
	if (respCol %in% names(outDf)) {
		if ("F" %in% keepCols & "XF" %in% names(outDf)) {
			if (report) cat("\n NOTE: Requested column 'F' - this has been renamed 'XF' in the data")
			keepCols [ keepCols == "F" ] <- "XF"
		}
		keepCols <- union(names(df), keepCols)
		colsThere <- keepCols %in% names(outDf)
		if (!all(colsThere)) {
			whichCols <- paste(keepCols [ !colsThere ], collapse=", ")
			ectdStop(paste("Some columns could not be found:", whichCols))
		}
		outDf <- outDf [ keepCols ]
		if (report) cat(paste("\n\n # Response variable '", respCol, "' was successfully created\n", sep=""))
	}
	else {
		cat("\n")
		ectdStop("Response variable could not be created")
	}
	outDf
}
