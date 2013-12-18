"createResponseVariable" <- function( 
	data,           #@ data frame to whoch to add the response variable
	equation,       #@ function for creting the response variable or text character
	preTest = TRUE, #@ run the pre test ?
	subsetSize = 5  #@ size used for the subset 
 ) { 
	################################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# createResponseVariable.R Tue Jun 19 15:20:10 BST 2007 @639 /Internet Time/
	#
	# Author: Romain
	################################################################################
	# DESCRIPTION: create the response
	# KEYWORDS: component:response
	################################################################################
          
	.requiredArgs(data)
	if (!is.data.frame(data)) ectdStop("`data` must be a data frame")
	nr <- nrow( data )

	eqFun <- try( match.fun( equation ), silent = TRUE )
	eqFun <- if( class(eqFun) == "try-error") {
		function( data ){
    		eval( parse( text = equation) ,data )
		}
	} else .checkFun( eqFun, "data")
   
	### run a pre test to find out if the equation will run a 
	### small subset of the data
	if( preTest && nr > subsetSize ){
		subsetData <- data[ 1:subsetSize , , drop=FALSE ]
		out <- try( eqFun( subsetData), silent = TRUE )
		if( class(out) == "try-error" ) ectdStop("Error when evaluating equation on subset of the data")    
		if( length(out) != subsetSize ){  #: testLength 
			ectdStop(
				"The equation given for the response does not generate" %.nt%
				"a vector of length equal to the dimension of the data" 
			)
		}
	}
  
	### call the equation on the data
	out <- try( eqFun(data), silent  = TRUE )
	if( class(out) == "try-error") ectdStop( "Error when executing the equation statement on the data")

	if( length(out) != nr ){
    	ectdStop(
   			"The equation given for the response does not generate" %.n%
			"a vector of length equal to the dimension of the data ($nr)" 
		)
	}
	out
}

