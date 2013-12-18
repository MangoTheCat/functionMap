"interimAnalysis" <- function(
	data,          				#@ Input dataset
	interimCode,    			#@ interim code
	uniqueDoses = NULL			#@ Unique study doses
){
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# interimAnalysis.R Wed Jun 27 12:36:28 BST 2007 @525 /Internet Time/
	#
	# Author: Romain    
	###############################################################################
	# DESCRIPTION: interim analysis support function
	# KEYWORDS: component:analysis
	###############################################################################

	# Check data input
	if (!is.data.frame(data)) ectdStop("Interim data must be a data frame") 

	# Return if no code
	if( missing(interimCode) || is.null(interimCode) ) return(list()) 

	# "Try" to execute the interim code
	interimCode <- try( match.fun (interimCode), silent = TRUE)
	if (class(interimCode) == "try-error") ectdStop("The interimCode function generated errors")
 
	# remove some na's
	data <- na.omit(data)
  
	# Try to excute the code on the (micro evaluation) data
	uniDoseTest <- "uniDoses" %in% names(formals(interimCode)) & length(uniqueDoses)
	if (uniDoseTest) out <- try( interimCode( data, uniDoses = uniqueDoses )  , silent = TRUE)
	else out <- try( interimCode( data )  , silent = TRUE)
	
	# Check output from the interim analysis
	.checkInterimObject(out)  
}

.checkInterimObject <- function(out) {
	if (class(out) == "try-error") ectdStop("Error when applying the interim code on the data")
	if (is.null(out)) out <- list()
	if (!is.list(out)) ectdStop("The output of the interim code must be a list structure (or NULL)")
	if (length(out) == 0 ) return(out)
	if (!all(names(out) %in% c("STOP", "KEEP", "DROP"))) ectdStop("List returned from interim step can only contain elements 'STOP', 'DROP' and 'KEEP'")
	if (length(out$STOP) && (!is.logical(out$STOP) || length(out$STOP) != 1)) ectdStop("'STOP' element in interim return list must be a single TRUE or FALSE")	
	if (length(out$DROP) && !is.numeric(out$DROP)) ectdStop("'DROP' element in interim return list must be a vector of dose levels")	
	if (length(out$KEEP) && !is.logical(out$KEEP)) ectdStop("'KEEP' element in interim return list must be a vector of logicals")
	if (all(c("DROP", "KEEP") %in% names(out))) ectdStop("Interim return list cannot contain both a 'DROP' element AND a 'KEEP' element")	
	out
}
