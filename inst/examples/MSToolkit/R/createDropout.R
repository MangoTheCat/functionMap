createDropout <- function(
  data,                             #@ Data Structure to which to add missing flag
  dropFunc,                         #@ Drop out function 
  seed = .deriveFromMasterSeed( ),  #@ Random seed
  idCol = getEctdColName("Subject"),                     #@ name of the subject varuable in the data  
  timeCol = getEctdColName("Time"),                 #@ name of the time column
  flagName = getEctdColName("Missing"),             #@ name of the missing variable to create
  ...                               #@ to pass to dropFunc
){
  ###############################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # createDropout.R Tue Jun 19 11:01:32 BST 2007 @459 /Internet Time/
  #     
  # Author: Romain    
  ###############################################################################
  # DESCRIPTION: add a misisng flag according to a user-supplied dropout function
  # KEYWORDS: component:data:missingness
  ###############################################################################
  
	set.seed( seed )
	flagName <- parseCharInput( flagName, expected = 1, convertToNumeric = FALSE, valid = TRUE)
	idCol    <- parseCharInput( idCol, expected = 1, convertToNumeric = FALSE, valid = TRUE)

	if( !missing(timeCol)){
		timeCol  <- parseCharInput( timeCol, expected = 1, convertToNumeric = FALSE, valid = TRUE)
		if (!(timeCol %in% names(data))) ectdStop("The `$timeCol` column is not in the supplied data")
	}
	if (!(idCol %in% names(data))) ectdStop("The `$idCol` column is not in the suppplied dataset")
  
	## Ensure the dropout function is valid
	checkDropOutFun( dropFunc, data, ... )
            
	### call the user-supplied function to get the missing flag
	missingFlag <- try( dropFunc( data, ... ), silent = TRUE )
	if (class(missingFlag) == "try-error") ectdStop("Error occuring when calling the dropout function: $missingFlag" ) 
  
	## Retain variable if time variable present
	if (timeCol %in% names(data) & any(missingFlag > 0)) {
		isMiss <- missingFlag > 0
    	minMiss <- tapply(data[isMiss, timeCol], data[isMiss, idCol], min)
    	missMatch <- minMiss[as.character(data[[idCol]])]
    	quickTest <- !is.na(missMatch) & data[[timeCol]] > missMatch
    	missingFlag <- as.numeric(quickTest | missingFlag)
	}

	## Add or modify the missing flag in the data set  
	data[[ flagName ]] <- if( flagName %in% names(data) ){
		1 * ( data[[ flagName ]] | missingFlag )   
	} 
	else missingFlag 
	data                          
}
