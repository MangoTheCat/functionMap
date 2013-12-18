createContinuousCovariates <- function(
  subjects,                      #@ Subjects for which to create covariates
  names,                         #@ Names for the continuous covariates
  mean,                          #@ Vector of means
  covariance = 1,                #@ Lower triangle of covariance matrix
  range = NULL,                  #@ Ranges of acceptable values for each covariates
  digits,                        #@ Number of digits used to round the values.
  maxDraws = 100 ,               #@ Maximum number of attempts allowed if initial\ 
                                 #  data not in range
  seed = .deriveFromMasterSeed(),#@ random seed to use
  idCol = getEctdColName("Subject"), 
  includeIDCol = TRUE
){
 	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# createContinuousCovariates.R Fri Jun 01 10:41:35 BST 2007 @445 /Internet Time/
	#
	# Author: Romain
	###############################################################################
	# DESCRIPTION: create a set of continuous covariates, from a (truncated) 
	#              multivariate nomal distribution
	# KEYWORDS: datagen, component:covariate
	###############################################################################
  
  set.seed( seed )
  
  ## sanity checks on the inputs
  if( missing(mean) ) 
    ectdStop("`mean` is needed in `createContinuousCovariates`")
  subjects <- .expandSubjects( subjects )
  nSubjects <- get("nSubjects")
  
  mean  <- parseCharInput( mean  )
  nCov <- length( mean )
  names <- if(missing(names)) { 
    "X" %.% 1:nCov
  } else { 
    parseCharInput( names , checkdup = TRUE, convertToNumeric = FALSE)
  }

	if(length(names) != length(mean))                                               
		ectdStop( 
        	"Dimension mismatch between `names` and `mean`"  %.nt%   
        	"`mean`  of length: " %.% length(mean) %.nt%
        	"`names` of length: "%.% length(names) ) 
	covariance <- parseCovMatrix( covariance, nCov )
	validNames( names, idCol )
	maxDraws <- as.integer(maxDraws)                      
	if( maxDraws < 1 ) ectdStop("The maximum number of draws should be a positive integer")

	if( !missing(digits) && digits < 0) ectdStop("The `digits` argument must be positive")
   
	if( is.null(range) ){
		out <- as.data.frame( mvrnorm( nSubjects, mu = mean, Sigma = covariance) )   
	} 
	else { # deal with range code                          
    	range <- parseRangeCode( range )    
    
    	nGen <- 0    
    	out <- do.call( data.frame, structure( rep(list(rep(0, nSubjects)), nCov), names = names) ) 
    	for( i in 1:maxDraws){       
    		# generate a new set of data
      		newsets <- as.data.frame( mvrnorm( nSubjects, mu = mean, Sigma = covariance) )
      		names( newsets ) <- names
      		alright <- try( eval( range, newsets) , silent = TRUE)
      		if (class(alright) == "try-error") next
      		indxs <- which(alright)
      		howManyToAdd <- min( nSubjects - nGen, length(indxs) )                   
      		if(howManyToAdd == 0) next    
      
			out[ nGen + 1:howManyToAdd, ] <- newsets[ indxs[1:howManyToAdd], ,drop = FALSE]
			nGen <- nGen + howManyToAdd
      
			.log( "..(createContinuousCovariates) iteration $i , $nGen generated (", 
				sprintf( "%6.2f", round(nGen / nSubjects * 100,2) ), "%)" )

			if(nGen == nSubjects) break
		}
		if( nGen != nSubjects ) ectdStop(paste("After", maxDraws, "attempts, covariate data for only", nGen, "of the", nSubjects, "subjects matched the range criteria"))
	}    
	names(out) <- names                                           
	out <- .roundIt( out, digits )

	if( includeIDCol ) out <- .eval( "data.frame( $idCol = subjects, out )" )
	out 
  
}
  
