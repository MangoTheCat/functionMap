createDirectories <- function(
  dirNames = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"), 		#@ A vector containing the full names of the directories to be created.
  workingPath = getwd(), 													#@ Directory in which to create directories
  warn = FALSE,          													#@ Should the dir.create function show warnings?
  method = getEctdDataMethod()   											#@ Data Storage Method to use
)
{
  ###############################################################################
  # Mango Solutions, Chippenham SN15 1BN 2009
  # createDirectories.R 03DEC09
  #
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Tries to create named subdirectories for storing replicate, micro 
  # evaluation and macro evaluation data.  Returns a logical vector representing
  # the success or failure of directory creation
  # KEYWORDS: IO
  ###############################################################################

  # Quit if the list is too short
  if(!length(dirNames)) ectdStop("No directories to create")

  # Match directory name against expected inputs
  dirNames <- match.arg(dirNames, several.ok = TRUE)

  # Match data storage method against recognised methods
  method <- match.arg(method, c("CSV", "RData", "Internal"))

  # Loop around if more than 1 directory
  if (length(dirNames) > 1) {
	 checkTrue <- c()
	 for (i in 1:length(dirNames)) checkTrue[i] <- createDirectories(dirNames[i], workingPath = workingPath, warn = warn, method = method)
	 return(checkTrue)
  }
  else {
	  # If we're only dealing with Micro/Macro, we should be using CSV method
	  if (substring(dirNames, 1, 1) == "M") method <- "CSV"

	  # Create physical directories
	  if (method %in% c("CSV", "RData")) {
		  
		  # Create full directory paths
		  fullPath <- file.path(workingPath, dirNames)
		  
		  # Create directories using the "dir.create" function
		  result <- dir.create(fullPath, showWarnings = warn)

		  # Log the creation of the directories
		  if (result) .log(paste("Created directory", fullPath))
		  return(result)
	  }
	  else return(TRUE)  # Don't need to create directories for "Internal" storage
  }	
}
