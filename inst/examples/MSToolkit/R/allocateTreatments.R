"allocateTreatments" <- function( 
 	trts,                 				#@    Number of treatments to which to allocate subjects
 	subjects,             				#@    Number of subjects to which to allocate treatments, or a vector of allocations
 	prop = NULL,      					#@    Proportions for sampling
 	ordered = FALSE,      				#@    Logical Flag, should allocations be assigned in order
 	seed = .deriveFromMasterSeed( ), 	#@ Random seed to allocate treatments
 	idCol = getEctdColName("Subject"),  #@     Subject variable name
	trtCol = getEctdColName("Trt")      #@     Treatment variable name
) {
	################################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# allocateTreatments.R 17/12/09
	#
	# Author: Rich P
	###############################################################################
	# DESCRIPTION: allocate treatments to subjects
	# KEYWORDS: datagen, component:data:allocate
	##############################################################################
  
	# Set the seed
	set.seed(seed)

	# Check inputs
	validNames( idCol, trtCol )  
	if(idCol == trtCol) ectdStop("`idCol` and `trtCol` should be different") 
	subjects <- parseCharInput( subjects )
  	trts     <- parseCharInput( trts ) 
	prop     <- parseCharInput( prop )
	
	# Create treatment vector
	if (length(trts) > 1) trts <- sort(unique(trts)) else trts <- 1:trts
	nTrts <- length(trts)
	
	# Check (or build) proportions
	if( is.null(prop) ) prop <- rep( 1/nTrts, nTrts)
	if( sum(prop) != 1 ) ectdStop( "`prop` does not sum up to one")
	if( length(prop) != nTrts) ectdStop( "`prop` should have the same length as the number of treatments: $trts" )    

	if( any(subjects < 0)) ectdStop( "Negative value in `subjects`")
	if( length(subjects) != 1 && length( subjects ) != nTrts) {
		ectdStop( "When providing a vector of `subjects`, it must be the same length as the number of treatments: $trts" )
	}
	
	# Perform the allocation
	nSubjects <- sum( subjects )
	alloc <- if( length(subjects) == 1 ) sample( trts, replace=TRUE, size = subjects, prob = prop)
	else rep( trts, subjects )

	if( !all(trts %in% unique(alloc))) ectdWarning( "Not all the treatments have been allocated")

	# Deal with ordered vs random
	if( ordered && is.unsorted(alloc) ) alloc <- sort( alloc )
	if( !ordered ) alloc <- sample( alloc )

	# Create output data frame
	outDf <- data.frame(1:sum(subjects), alloc)
	names(outDf) <- c(idCol, trtCol)
	outDf

}

