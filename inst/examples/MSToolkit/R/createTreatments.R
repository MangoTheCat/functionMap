createTreatments <- function( 
		doses,                  #@ Treatment doses
		times = NULL,           #@ Treatment time points
		type = "Parallel",      #@ Treatment type: Parallel  or Crossover
		sequence,               #@ Treatment matrix for crossover designs
		doseCol = getEctdColName("Dose"),       #@ Dose variable name
		timeCol = getEctdColName("Time"),       #@ Time variable name
		trtCol  = getEctdColName("Trt")         #@ Treatment variable name
){
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# createTreatments.R Fri Jun 01 11:47:08 BST 2007 @491 /Internet Time/
	#
	# Author: Romain           
	###############################################################################
	# DESCRIPTION: creates a data frame of all possible treatments for a given scenario
	# KEYWORDS: datagen, component:data:treatment
	###############################################################################
	
	# Derive the treatment type
	type <- initialChar( type, "pc", "'type' should be Parallel or Crossover" )      
	if(!missing(sequence) ) {
		type <- "c"
	} else if( !missing(doses) ) {
		type <- "p"
	} else {
		ectdStop("Need arguments 'sequence' or 'doses'")   #: doseOrSequence
	}
	
	# tidy up the 'times' argument for crossover
	# and check about the sequence matrix
	if(type == "c"){  
		if(missing(sequence)) # not gonna happen {doseOrSequence}
			ectdStop("'sequence' must be supplied for a Crossover design")
		if(!is.matrix(sequence) || !is.numeric(sequence) )
			ectdStop("'sequence' must be a numeric matrix")
		if(is.null(times)) times <- 1:nrow(sequence)
	}
	
	# tests for Parallel type
	if(type == "p" && missing(doses)) 
		ectdStop("'doses' must be supplied for parallel treatment")
	
	doseCol <- parseCharInput( doseCol, convertToNumeric = FALSE, valid = TRUE, expected = 1 )
	timeCol <- parseCharInput( timeCol, convertToNumeric = FALSE, valid = TRUE, expected = 1 )
	trtCol <- parseCharInput(  trtCol, convertToNumeric = FALSE, valid = TRUE, expected = 1 )
	
	times <- parseCharInput( times )
	nTimes <- length(times)
	
	if( type == "p" ){ # then make the sequence matrix
		doses <- parseCharInput( doses )
		if( is.null(times) ) {
			sequence <- matrix( doses, nrow = 1)
		} else { 
			sequence <- matrix( doses, nrow = length(times), 
					ncol = length(doses), byrow = TRUE )
			sequence[ times < 0, ] <- 0
		}
	}
	
	if( type == "c"){ 
		
		# does it have the right number of rows
		if( nTimes != nrow(sequence)  ) {
			diffSeq <- nTimes - nrow(sequence)
			if (diffSeq > 0 && all(times[1:diffSeq] <= 0)) sequence <- rbind( matrix(0, nrow=diffSeq , ncol = ncol(sequence)), sequence)
			else {
				ectdStop( 
						"difference between the number of rows in the sequence matrix" %.n%
								"and the number of time points\n" )
			}
		}
		
		# no dose run-in period           
		if( any(runinTimes <- times < 0) && any( sequence[which(runinTimes),] != 0 ) )
			ectdStop( "The sequence matrix suggests a dose run-in period" ) 
		
	}
	### from this point, everything has been checked, everything should 
	### be alright to build the treatment data
	
	nTreat <- ncol(sequence)
	
	out <- .eval( 
			if( type =="p" && is.null(times) ) {  
						"data.frame( $trtCol=1:nTreat, $doseCol=as.vector(sequence)  ) "
					} else {
						"data.frame( $trtCol=rep(1:nTreat, each=nTimes) , $timeCol = rep( times, nTreat), $doseCol = as.vector(sequence)  ) "
					} )
	
	out
	
}   

