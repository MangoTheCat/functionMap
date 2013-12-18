"macroEvaluation" <- function(
	data,    		               				#@ dataset
	macroCode,      				        	#@ macro code
	interimCol = getEctdColName("Interim"), 	#@ name of the INTERIM column 
	doseCol = getEctdColName("Dose")        	#@ name of the DOSE column
)
{
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# macroEvaluation.R Wed Jun 27 14:32:46 BST 2007 @606 /Internet Time/
	#
	# Author: Romain/Rich P 
	###############################################################################
	# DESCRIPTION: summarise a single set of 
	# KEYWORDS: component:analysis
	###############################################################################
	.log( "Calling macro evaluation function" )

	# Checks on inputs
	if(!is.data.frame(data) || !nrow(data)) ectdStop("Input data must be a data frame with at least 1 row")
	.checkCharacter(interimCol, doseCol)					# Check (character) inputs
	if (!is.function(macroCode)) ectdStop("Macro evaluation code must be a function")

	# Prepare the arguments for the macro evaluation call
	funArgs <- names(formals(macroCode))
	callList <- list(data)
	if ("doseCol" %in% funArgs) {
		checkColNames(names(data), doseCol)
		callList$doseCol <- doseCol
	}
	if ("interimCol" %in% funArgs) {
		checkColNames(names(data), interimCol)
		callList$interimCol <- interimCol
	}
	# Try to run the code on the data
	out <- try( do.call(macroCode, callList), silent = TRUE)
	if(class(out) == "try-error") ectdStop("Error when calling the macroCode \n\t$out")
	
	# Check structure and return
	.log("Checking macro evaluation data")
	checkMacroFormat(out)
	out  
}
