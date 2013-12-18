"ectdStop" <- 
		function (msg, call. = TRUE, domain = NULL, verbose = getEctdVerbose()) 
{
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# ectdStop.R 02DEC09
	# Author: R.Pugh           
	###############################################################################
	# DESCRIPTION: generate error message
	# KEYWORDS: error, component:support
	###############################################################################

	try(msg <- if (verbose) 
						.strinterp(msg) %.n% "----------------------------------------------------------------------" %.n% 
								paste(sapply(sys.calls(), function(x) as.character(x[[1]])), 
										collapse = " > ") %.n% "----------------------------------------------------------------------"
					else .strinterp(msg), silent = TRUE)
	stop(msg)
}

