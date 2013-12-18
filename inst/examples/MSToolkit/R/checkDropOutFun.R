"checkDropOutFun" <- function(
	fun,          			#@ dropout function to check
	data,         			#@ Data on which to execute function
	sizeSubset = 5, 		#@ size of the subset to use to perform the check
	useSubset = TRUE, 		#@ Logical flag, do we subset before performing the check
	...           			#@ Extra arguments to pass to the function
){
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# checkDropOutFun.R Tue Jun 19 11:12:38 BST 2007 @467 /Internet Time/
	#     
	# Author: Romain/Rich P
	###############################################################################
	# DESCRIPTION: check the validity of the drop out function
	# KEYWORDS: component:support
	###############################################################################
	
	## make sure it is a function
	fun <- try( match.fun(fun), silent = TRUE )
	if (class(fun) == "try-error") ectdStop("Dropout function could not befound")
	if (!is.function(fun)) ectdStop("Dropout function is not a function")

	# Check for a data argument
	nf <- names( formals( fun ) )  
	if (!any(nf == "data")) ectdStop("The drop out function must have a `data` argument")
  
	# Run function on section of data
	hd <- if( useSubset ) head( data, n = sizeSubset  ) else data
	out <- try( fun( hd, ... ) , silent = TRUE) 
	if (class(out) == "try-error") ectdStop("Error when calling the dropout function on a subset of data")

	# Check the output from the function
	if (length(out) != nrow(hd )) ectdStop("The Dropout function outputs a vector of wrong length")
	if (!all(unique( as.integer( out )) %in% 0:1)) ectdStop("The dropout function outputs a vector with values different from 0 and 1")

	# Return TRUE flag if we've got this far
	invisible( TRUE )
}
