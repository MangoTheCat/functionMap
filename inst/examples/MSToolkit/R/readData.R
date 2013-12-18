readData <- function(
  dataNumber,               #@ The number of the data entry, should be between 1 and 9999
  dataType = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"),        #@ String containing the type of data to be read ("Replicate", "Micro" or "Macro)
  variables = NULL,         #@ The variables we are expecting in the data
  workingPath = getwd(),     #@ Working directory
  method = getEctdDataMethod()
  )
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # readData.R Fri Jun 22 15:47:41 BST 2007 @418 /Internet Time/
  #
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Reads an individual data entry of the specified type (Replicate, Micro or Macro) and returns it as a data frame
  # KEYWORDS:IO
  ###############################################################################
{
	
  # Check data type
  dataType <- match.arg(dataType)
  if (dataType != "ReplicateData") method <- "CSV" 
	
  # Basic input checks
  .checkCharacter(dataType, method, workingPath)		# Check (character) inputs
  .checkNumeric(dataNumber)								# Check (numeric) input
  

  data <- switch(method, 
	"CSV" = {
		# Get the location of the file containing the specified data element
		fullPath <- .dataGetFullPath(dataNumber = dataNumber, dataType = dataType, workingPath = workingPath, method = method) 
		# Read the data
		try(.readAndCheckInputFile(fullPath, variables))
	}, 
	"RData" = {
		# Get the location of the file containing the specified data element
		fullPath <- .dataGetFullPath(dataNumber = dataNumber, dataType = dataType, workingPath = workingPath, method = method) 
		# Read the data
		try(.readFromRData(fullPath, variables))
	},
	"Internal" = {
		try(.ectdEnv$DataStore[[dataNumber]])
	}, 
	ectdStop("Did not recognise data storage method")
  )
	
  # Check and return structure
  if (!is.data.frame(data)) ectdStop(paste("Could not read data replicate", dataNumber))
  data
}
