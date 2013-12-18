"addResidualError" <- function(
	response,                       								#@ numeric vector of response data
	covariance,                     								#@ lower triangle or matrix
	errStruc = c("Additive", "Proportional", "Log-Normal"),			#@ function describing how to apply residual error
	seed = .deriveFromMasterSeed( )									#@ Random Seed to use
) { 
	################################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# addResidualError.R Tue Jun 19 16:17:20 BST 2007 @678 /Internet Time/
	#
	# Author: Romain/Rich P
	################################################################################
	# DESCRIPTION: add residual error to a response
	# KEYWORDS: component:response
	################################################################################

	.requiredArgs(response, "The 'response' variable is required")
	.requiredArgs(covariance, "The 'covariance' argument is required")
	set.seed(seed)

	# <TODO>
	# currently handle only one variable
	covariance <- parseCovMatrix( covariance, 1)                  
	# </TODO>

	errFun <- if(is.function(errStruc)) errStruc 
	else {
		getErrStruc <- try(match.arg(errStruc))
		if (class(getErrStruc) == "try-error") {
			errStruc <- casefold(substring(errStruc, 1, 1), upper = TRUE)
			getErrStruc <- match.arg(errStruc, c("Additive", "Proportional", "Log-Normal"))
		}
		switch( getErrStruc, 
			"Additive"       = function(x,y) x+y,      			# Additive
       		"Log-Normal"     = function(x,y) x * exp(y),  		# Log-Normal
			"Proportional"   = function(x,y) x * (1 + y)  		# Proportional
		)
	}
  
	error <- rnorm( length(response), mean = 0, sd = sqrt(covariance[1,1]) )
  
	if( length(formals(errFun)) < 2  ) ectdStop("The error function should take at least two arguments")
  
	out <- errFun( response, error )
	if( !is.numeric(out)) ectdStop("The error function should return a numeric vector") 
	if( length(out) != length(response)){     
    	ectdStop(
    		"The error function supplied generates a vector that does not have" %.nt%
      		"the same length as the response vector supplied" 
		) 
	}
	out  
}

