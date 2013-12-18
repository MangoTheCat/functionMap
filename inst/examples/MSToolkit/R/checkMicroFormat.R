"checkMicroFormat" <- function (
  data,                  # A data frame which will be checked for the correct micro evaluation data format
  doseCol = getEctdColName("Dose"),
  mustHaveDose = FALSE
) {
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# checkMicroFormat.R Wed Jun 27 14:54:17 BST 2007
	# Author: Francisco
	###############################################################################
	# DESCRIPTION: Checks whether or not a given data frame is of the correct format 
	# for a micro evaluation data entry
	# KEYWORDS:  
	# Documented in Support Functions Design Specification 
	###############################################################################

	# Check data structure
	if(!is.data.frame(data)) ectdStop("Output from analysis must be a data frame")
	if(!nrow(data)) ectdStop("Output from analysis must be a data frame with at least 1 row")
  
	# Check if "Dose" column exists
	if (mustHaveDose) {
		doseCol <- parseCharInput( doseCol, convertToNumeric = FALSE, expected = 1, valid = TRUE )
		if (!(doseCol %in% names(data))) {
			caseTest <- casefold(names(data)) == casefold(doseCol)
			if (any(caseTest)) names(data)[caseTest] <- doseCol
			else ectdStop("Output from analysis must contain a 'DOSE' column")
		}
	}

	# Return data
	data
}
