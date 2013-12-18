"checkSimAlpha" <- function(alpha = 95)
{
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	#
	# Author: Rich
	###############################################################################
	# DESCRIPTION: Utility function to parse "alpha" input
	###############################################################################
	if(length(alpha) > 1) ectdStop("Only 1 alpha should be provided")
	if(is.character(alpha)) {
		alpha <- gsub(" ", "", alpha)
		suppressWarnings(alpha <- as.numeric(gsub("%", "", alpha)))
		if(any(is.na(alpha))) ectdStop("Could not parse specified alpha")
	}
	if(alpha > 1) alpha <- alpha/100
	if(alpha < 0.5) alpha <- 1 - alpha
	max(min(alpha, 1), 0)
}
