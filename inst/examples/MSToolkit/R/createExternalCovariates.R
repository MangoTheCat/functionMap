createExternalCovariates <- function(
  subjects,       #@ Subjects for which to create covariates
  names,          #@ Names for the continuous covariates
  file,           #@ File from which to import (including full or relative path)
  sameRow = TRUE, #@ 
  subset = NULL,  #@ Subset to apply to the data
  refCol = NULL,  #@ Parameter reference variable in dataset
  dataId = idCol, #@ Subject variable in data
  idCol = getEctdColName("Subject"),   #@ Subject variable name
  percent = 20,   #@ Percentage of values to use for checking rows in dataset vs samples to extract
  seed = .deriveFromMasterSeed(), # Random seed
  includeIDCol = TRUE, 
  refColSuffix = "refCol",
  workingPath = getwd()
){
 	###############################################################################
	# � Mango Solutions, Chippenham SN14 0SQ 2006
	# createExternalCovariates.R Fri Jun 01 10:43:38 BST 2007 @446 /Internet Time/
	#
	# Author: 
	###############################################################################
	# DESCRIPTION: import a set of covariates from a file
  # KEYWORDS: datagen, io, component:covariate
	###############################################################################
  
  set.seed( seed )
  subjects <- .expandSubjects( subjects )
  nSubjects <- get("nSubjects")
  names    <- parseCharInput(names, convertToNumeric = FALSE, checkdup = TRUE)
  subset   <- parseRangeCode( subset )
  validNames( idCol, dataId, names)
  if(!is.null(refCol)) validNames( refCol )
  percent <- parseCharInput( percent, expected = 1, convertToNumeric = TRUE )
  if( percent < 0 || percent > 100) 
    ectdStop("`percent` should be between 0 and 100")
  
  iData <- .readAndCheckInputFile( file.path(workingPath, file), c(dataId, names) )
  if(!is.null(refCol) && refCol %!in% names(iData) )
    ectdStop("There is no column `$refCol` in the dataset `$file`")
  if(!is.null(refCol) && !sameRow )
    ectdStop("sameRow = FALSE is not compatible with the use of refCol")
  if( !is.null(subset)) iData <- .applyDataSubset(iData, subset)

  # taking the first value for each ID
  iData <- iData[ !duplicated(iData[[dataId]]), ,drop = FALSE]
   
  if( nrow(iData) < percent * nSubjects / 100  )
    ectdWarning("Less than $percent % of lines in the dataset compared to the number of subjects to sample")
 
  if(sameRow){
    idx <- sample( nrow(iData), replace = TRUE, size = nSubjects )
    out <- iData[ idx, c(names, refCol), drop = FALSE ] 
    if(!is.null(refCol)) names(out)[length(out)] <- names(out)[length(out)] %.% '.' %.% refColSuffix
    
    ## add the ID variable
    if( includeIDCol ) out <- .eval( "data.frame( $idCol = subjects, out)" )  
    
  }  else {                                 
    out <- as.data.frame( matrix(NA, nrow = nSubjects, ncol = length(names)+includeIDCol ) )
    if( includeIDCol ){
      names(out) <- c( idCol, names)
      out[[idCol]] <- subjects
    } else { 
      names(out) <- names
    }
    for( nm in names) out[[ nm ]] <- sample( iData[[nm]], size= nSubjects, replace = TRUE)
  }
  rownames( out ) <- 1:nSubjects
  out  
}

