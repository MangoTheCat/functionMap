"checkMacroFormat" <- function(
	data    #@ Data to check
)
{
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# Fri Jun 21 16:51 BST 2007 @445 /Internet Time/
	#
	# Author: Francisco/Rich P
	###############################################################################
	# DESCRIPTION: Checks whether a data frame conforms with the expected 
	# "Macro Evaluation" data format
	# KEYWORDS:misc, IO 
	###############################################################################  
	if(!is.data.frame(data) || nrow(data) != 1) ectdStop("Macro evaluation data must be a data frame with a single row")
	invisible(TRUE)
}
  