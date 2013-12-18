createCovariates <- function(
  subjects,     #@ Subjects for which to create covariates
  
  ## arguments for the `createContinuousCovariates` function
  conNames=NULL,#@ Continuous covariate names
  conMean,      #@ Continuous covariate means
  conCov,       #@ Continuous covariate covariance matrix
  conRange=NULL,#@ Continuous covariate acceptable range
  conDigits,    #@ Continuous covariate rounding digits
  conMaxDraws=100,#@ Continuous covariate maximum draws
 
  ## arguments for the `createDiscreteCovariates` function
  disNames=NULL,#@ Discrete covariate names
  disValues,    #@ Discrete covariate values
  disProbs,     #@ Discrete covariate probabilities
  disProbArray, #@ Array of probabilities for multivariate sampling

  ##arguments for the `createExternalCovariates` function
  extNames=NULL,#@ Names for the continuous covariates
  extFile,      #@ File from which to import (including full or relative path)
  extSubset,    #@ Subset to apply to data
  extRefCol,    #@ Reference variable
  extSameRow=TRUE,   #@ Logical flag: should covariates sampled be from the same row
  extDataId=idCol, #@ Subject variable name from file
  workingPath = getwd(), #@ Working directory
  
  ## arguments for the `createTimeVaryingCovariates` function
  timeNames=NULL,
  timeMean,
  timeCov,
  timeRange=NULL,
  timeCol = getEctdColName("Time"),
  timePeriod,
  
  ## common args
  idCol = getEctdColName("Subject"),  #@ Subject variable name for return data
  seed=.deriveFromMasterSeed() #@ random seed
){
 	###############################################################################
	# � Mango Solutions, Chippenham SN14 0SQ 2006
	# createCovariates.R Fri Jun 01 10:44:40 BST 2007 @447 /Internet Time/
	#
	# Author: Romain
	###############################################################################
	# DESCRIPTION: covariate component, wrapper for the functions: 
  #              createContinuousCovariates, createExtenalCovariates, 
  #              createDiscreteCovariates
  # KEYWORDS: datagen, component:covariate
	###############################################################################
  
  set.seed( seed )
           
  subjects <- .expandSubjects( subjects )
  idCol    <- parseCharInput( idCol, convertToNumeric = FALSE, expected = 1, valid = TRUE)
  timeCol  <- parseCharInput( timeCol, convertToNumeric = FALSE, expected = 1, valid = TRUE)
  
  conNames <- parseCharInput( conNames, convertToNumeric = FALSE, checkdup = TRUE ) 
  extNames <- parseCharInput( extNames, convertToNumeric = FALSE, checkdup = TRUE ) 
  disNames <- parseCharInput( disNames, convertToNumeric = FALSE, checkdup = TRUE ) 
  timeNames <- parseCharInput( timeNames, convertToNumeric = FALSE, checkdup = TRUE ) 
  if( any(duplicated(c(conNames, extNames, disNames, timeNames))))
    ectdStop("duplicated names in `conNames`, `extNames`, `disNames`, `timeNames`")

  ## calling the createContinuousCovariates function
  dataList <- NULL
  dataList[[idCol]] <- .eval( "data.frame( $idCol = subjects)" )
   
  dataList$continuous <- if( !is.null(conNames) ){
    conArgs <- list( 
      subjects = subjects,  names    = conNames, 
      idCol    = idCol,  seed     = seed, 
      range    = conRange, maxDraws = conMaxDraws, 
      includeIDCol = FALSE)
    if(!missing(conMean))    conArgs$mean       <- conMean
    if(!missing(conCov))     conArgs$covariance <- conCov
    if(!missing(conDigits))  conArgs$digits     <- conDigits
    
    do.call( createContinuousCovariates, conArgs)
  }  
  
  ## calling the createExternalCovariates function
  dataList$external <- if( !is.null(extNames) ){
    extArgs <- list( subjects = subjects, names = extNames, idCol = idCol, 
      seed = seed, sameRow = extSameRow, dataId = extDataId, 
      includeIDCol = FALSE, workingPath = workingPath )
    if(!missing(extFile) )    extArgs$file   <- extFile
    if(!missing(extSubset) )  extArgs$subset <- extSubset
    if(!missing(extRefCol) )  extArgs$refCol <- extRefCol
     
    do.call( createExternalCovariates, extArgs)
  }

  ## calling the createDiscreteCovariates function
  probCall <- !missing(disProbArray) && length(disProbArray)
  dataList$discrete <- if( !is.null(disNames) | probCall){
    disArgs <- list( subjects = subjects, idCol = idCol, seed = seed, includeIDCol = FALSE )
		if( !missing(disNames    )) disArgs$names     <- disNames
		if( !missing(disValues   )) disArgs$values    <- disValues   
		if( !missing(disProbs    )) disArgs$probs     <- disProbs    
		if( !missing(disProbArray)) disArgs$probArray <- disProbArray
		do.call( createDiscreteCovariates, disArgs)
  }
  
  names( dataList )  <- NULL  
  out <- do.call(data.frame, dataList[!sapply(dataList, is.null)]  )
  
  out.time <- if( !is.null(timeNames) ){
	  timeArgs <- list( 
			  subjects = subjects,  names    = timeNames, 
			  idCol    = idCol,  seed     = seed, 
			  range    = timeRange, maxDraws = conMaxDraws, 
			  timeCol = timeCol)
	  if(!missing(timeMean))    timeArgs$mean       <- timeMean
	  if(!missing(timeCov))     timeArgs$covariance <- timeCov
	  if(!missing(conDigits))  timeArgs$digits     <- conDigits
	  if(!missing(timePeriod))  timeArgs$treatPeriod     <- timePeriod
	  
	  do.call( createTimeVaryingCovariates, timeArgs)
  }  
  
  if (!is.null(out.time)) out <- merge(out.time, out)
  
  out 
  
}



