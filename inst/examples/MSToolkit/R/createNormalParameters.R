"createNormalParameters" <- function( 
	subjects,             						#@ subjects for which to create parameters
	names,                						#@ names of parameters to generate
	mean,                 						#@ means for fixed parameters
	covariance = 0,       						#@ covariance matrix for fixed parameters
	range,                						#@ Range of acceptable values for parameters
	betNames,             						#@ between subject effects to create
	betMean = 0,          						#@ means for between subject effect
	betCov = 0,           						#@ covariance for the between subject effect
	errStruc = c("None", "Proportional", "Additive", "Log-Normal"),		#@ function to map effect (Additive, Proportional, Log-Normal or None)
	suffix = ".Between",  						#@ Suffix to use for retain between subject effects
	idCol = getEctdColName("Subject"),       	#@ ID variable name for return data
	maxDraws = 10,        						#@ Maximum number of iterations for valid parameters
	seed = .deriveFromMasterSeed( ),  			#@ Random seed
	flagName = getEctdColName("ParOmit"), 		#@ Parameter omit flag name
	digits = 3,  								#@ number of digits to round to
	parRangeTolerance = .5						#@ Percentage of subjects with acceptable inputs for which we should continue 
) {
	################################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# createNormalParameters.R Wed Jun 20 11:39:52 BST 2007 @486 /Internet Time/
	#
	# Author: Rich P
	###############################################################################
	# DESCRIPTION: create parameters from a normal distribution
	# KEYWORDS: datagen, component:data:allocate
	##############################################################################
  
  	# Set the seed
	set.seed(seed)
  
	## initial tests
	subjects <- .expandSubjects( subjects ) 
	nSubjects <- length(subjects)
	mean <- parseCharInput( mean )
	nFixed <- length(mean)
	idCol <- parseCharInput( idCol, expected = 1, convertToNumeric = FALSE, valid = TRUE)
	flagName <- parseCharInput( flagName, expected = 1, convertToNumeric = FALSE, valid = TRUE)
	maxDraws <- parseCharInput( maxDraws, expected = 1, convertToNumeric = TRUE)
	digits <- parseCharInput( digits, convertToNumeric = TRUE)
	
	names <- if(missing(names)) {
    	"X" %.% 1:nFixed 
	} else { 
    	parseCharInput( names, convertToNumeric = FALSE, checkdup = TRUE, expected = nFixed, valid = TRUE )
	}
	covariance <- parseCovMatrix( covariance, nFixed )

	# Checks on digits, maxDraws and errStruc
	getErrStruc <- try(match.arg(errStruc))
	if (class(getErrStruc) == "try-error") {
		errStruc <- casefold(substring(errStruc, 1, 1), upper = TRUE)
		getErrStruc <- try(match.arg(errStruc))
		if (class(getErrStruc) == "try-error") ectdStop("Error Structure input should be 'None', 'Additive', 'Log-Normal' or 'Proportional'")
	}
	if (maxDraws < 1) ectdStop("The maximum number of draws should be a positive integer")
	if (any(digits < 0)) ectdStop("The `digits` argument must be positive")

	## Generate parameters depending on presence of "range" input
	if (missing(range)) {

		# Generate 1 set of fixed effects	
		fixed <- .generateFixedPars(mean, covariance, nSubjects, names)		# Fixed effects
	
		## Now deal with the between subject effects
		if ( !missing(betNames) ) {

			# Generate a set of between subject parameters
			betNames <- parseCharInput( betNames, convertToNumeric = FALSE, checkdup = TRUE, valid = TRUE )
			between <- .generateBetweenPars(betNames, names, betMean, betCov, nSubjects, suffix)
			omitFlag <- rep(0, nSubjects)
			out <- .combineFixedAndBetween(fixed, between, betNames, getErrStruc, suffix, digits)

		} 
		else {   # no between
			out <- .roundIt( fixed, digits )
		}

		# Combine data
		out <- data.frame( subjects, out, rep(0, nSubjects))
		names(out)[c(1, length(out))] <- c(idCol, flagName)

	}
	else { 
		# Loop over draws allowed
		for (i in 1:maxDraws) {
			fixed <- .generateFixedPars(mean, covariance, nSubjects, names)		# Fixed effects
			
			## Now deal with the between subject effects
			if ( !missing(betNames) ) {
				
				# Generate a set of between subject parameters
				betNames <- parseCharInput( betNames, convertToNumeric = FALSE, checkdup = TRUE, valid = TRUE )
				between <- .generateBetweenPars(betNames, names, betMean, betCov, nSubjects, suffix)
				omitFlag <- rep(0, nSubjects)
				out <- .combineFixedAndBetween(fixed, between, betNames, getErrStruc, suffix, digits)
				
			} 
			else {   # no between
				out <- .roundIt( fixed, digits )
			}
			
			# Combine data
			out <- data.frame( subjects, out, rep(0, nSubjects))
			names(out)[c(1, length(out))] <- c(idCol, flagName)
			
			# Check to see if the data is in range
			out <- .checkParameterRange(out, range, flagName)
			if (all(out[[flagName]] == 0)) break

			# Stop if we have exhausted our number of draws
			if (i == maxDraws) {
				subjectFlags <- out[[flagName]][!duplicated(out[[idCol]])]
				propOk <- 1 - sum(subjectFlags) / length(subjectFlags)
				roundPercent <- round(100 * propOk, 1)
				basicMessage <- paste("After ", maxDraws, " attempts, ", roundPercent, "% of subjects have parameters in acceptable range", sep="")
				if (propOk >= parRangeTolerance) ectdWarning( basicMessage )
				else ectdStop( basicMessage )
			}
		}
	}

	rownames(out) <- 1:nSubjects
	out
}

".generateFixedPars" <- function(mean, cov, N, colNames) {
	fixed <- as.data.frame( matrix( mvrnorm( n = 1, mu = mean,  Sigma = cov ), nrow = 1 ) )
	names(fixed) <- colNames
	fixed[rep(1, N), , drop = FALSE]
}

".generateBetweenPars" <- function(betNames, fixedNames, mean, cov, N, suffix) {

	myTest <- betNames %in% fixedNames
	if ( !all(myTest)) {
		outMessage <- paste("Some between subject effects don't have a corresponding fixed parameter:", paste(betNames[!myTest], collapse=", "))
		ectdStop( outMessage )
	}

	# Get the covariance matrix + means
	nBetween <- length( betNames )
	betCov   <- parseCovMatrix(cov, nBetween )
	betMean <- rep(parseCharInput(mean), length.out = nBetween)

	# Generate the data once
	between <- as.data.frame( mvrnorm( N, mu = betMean, Sigma = betCov ) )

	## apply the suffix to the between data and return it
	names( between ) <- paste(betNames, suffix, sep="")
	between	
}

".combineFixedAndBetween" <- function(fixed, between, betNames, errStruc, suffix, digits) {

	switch( errStruc, 
		"Additive" = {                    # Additive: fixed + between 
			out <- fixed
			for ( bn in betNames ) out[[ bn ]] <- out[[ bn ]] + between[[ paste(bn, suffix, sep="") ]] 
			out <- .roundIt( out, digits )
		}, 
		"Log-Normal" = {                    # Log Normal: fixed * exp ( between )
			out <- fixed
			for( bn in betNames ) out[[ bn ]] <- out[[ bn ]] * exp( between[[ paste(bn, suffix, sep="") ]] ) 
			out <- .roundIt( out, digits )
		}, 
		"Proportional" = {                    # Proportional: fixed * (1 + between)
			out <- fixed
			for( bn in betNames ) out[[ bn ]] <- out[[ bn ]] * ( 1 + between[[ paste(bn, suffix, sep="") ]] )
			out <- .roundIt( out, digits )
		}, 
		"None" = {                    # keep fixed and between
			digits <- parseCharInput( digits, convertToNumeric = TRUE)
			fixed   <- .roundIt( fixed, digits )
			if ( length(digits) == 1 || ncol(fixed) == ncol(between) ) between <- .roundIt( between, digits)
			else between <- .roundIt( between, digits = digits[ names(fixed) %in% betNames ] )
			out <- data.frame( fixed, between) 
		}
	)
	out
}

".checkParameterRange" <- function(out, range, flagName) {
	
	range <- parseRangeCode( range )		# Parse the range code
	alright <- try( eval( range, out ) )
	if (class(alright) == "try-error" || length(alright) != nrow(out)) out[[flagName]] <- rep(1, nrow(out))
	else out[[flagName]] <- as.numeric(!alright)
	out

}
