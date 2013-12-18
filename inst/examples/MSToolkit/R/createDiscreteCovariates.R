createDiscreteCovariates <- function(
  subjects,     #@ Subjects for which to create covariates
  names,        #@ Names for the continuous covariates
  values,       #@ Values from which to sample for each covariate
  probs,        #@ Probabilities for each variable
  probArray,    #@ An array of probabilities for multivariate sampling
  seed  = .deriveFromMasterSeed( ), #@ Random number generation seed
  idCol = getEctdColName("Subject"), 
  includeIDCol  = TRUE
)
{
 	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# createDiscreteCovariates.R Fri Jun 01 10:42:47 BST 2007 @446 /Internet Time/
	#
	# Author: 
	###############################################################################
	# DESCRIPTION: create a set of discrete covariates
    # KEYWORDS: datagen, component:covariate
	###############################################################################
                           
	set.seed(seed)
	names  <- if( missing(names) && !missing(probArray) ) {
				if (is.array(probArray)) ectdStop("If probArray is supplied as an array, the names input must be provided")
				colnames( probArray )[-ncol(probArray)]
			} else {
				parseCharInput( names  , convertToNumeric = FALSE, checkdup = TRUE)
			}
	validNames( names, idCol )
  
	## handle `probs` or `probArray`
	if( missing(probs) ){
		if(missing(probArray)){
			ectdStop("need one of `probArray` or `probs`")
		}     
		# Attempt to deduce values input from the probArray argument
		if( missing(values)){ 
			if( is.array( probArray) ){
				values <- dimnames(probArray)
				if (!length(values)) ectdStop("If probArray input is supplied as an array, either the array must have dimension names or the 'values' input must be provided")
				values <- lapply(values, function(x) {
							numTry <- suppressWarnings(as.numeric(x))
							if (any(is.na(numTry))) return(x) else return(numTry)
						}
				)
			} else {
				values <- lapply( probArray[,-ncol(probArray),drop = FALSE], unique )
			} 
		} else {
			values <- parseHashString( values )
		}
		if( !.allSameLength(values, names) ) ectdStop( "`names` and `values` inputs must have the same length") 
	} else {
		values <- parseHashString( values, convertToNumeric = FALSE)
		probs  <- parseHashString( probs, checkProb = TRUE  )
		if( !.allSameLength(values, probs, names) ) ectdStop( "`names`, `probs` and `values` must have the same length")
		if( any(sapply(probs,length) != sapply(values, length)) ) ectdStop("items in `values` and `probs` must have the same length")
		names(probs)  <- names
	}
 
	names(values) <- names
    
	subjects <- .expandSubjects( subjects )
	nSubjects <- get("nSubjects")

	grid <- .handleProbArray( probArray, values, probs)

	indexes <- sample( 1:nrow(grid), prob = grid[,ncol(grid)], size = nSubjects , replace = TRUE)
	out <- grid[indexes, names, drop = FALSE]
	
	if( includeIDCol ) out <- .eval( "data.frame( $idCol = subjects, out)" )
	
	rownames(out) <- 1:nSubjects
	out
}
