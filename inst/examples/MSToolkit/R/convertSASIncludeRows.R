"convertSASIncludeRows" <- function(
		includeRows = NULL,    #@ matrix of 2 columns describing the changes to make to the data
		doseCol = getEctdColName("Dose"),
		interimCol = getEctdColName("Interim")	
){
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# convertSASIncludeRows.R 15DEC09
	#
	# Author: Rich    
	###############################################################################
	# DESCRIPTION: Write "drop doses" code in "SAS" format
	# KEYWORDS: component:analysis
	###############################################################################
	
	# Parse inputs
	if (!length(includeRows)) return("**** No doses to drop ****;")
	if (!is.matrix(includeRows) || ncol(includeRows) != 2) ectdStop("'includeRows' input should be a matrix with 2 columns")
	
	# Inner paste subset function
	"innerPasteSASSubset" <- function(vec, interimCol, doseCol) {
		paste("(", interimCol, "=", vec[1], "and", doseCol, "=", vec[2], ")")
	}
	
	# Perform parsing
	outSub <- apply(includeRows, 1, innerPasteSASSubset, interimCol = interimCol, doseCol = doseCol)
	outSub <- paste(outSub, collapse = " or ")
	paste("IF", outSub, ";")
	
}
