.dataGetFullPath <- function (
  dataNumber,               #@ Number of data entry, should be between 1 and 9999
  dataType = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"),    #@ Type of data, should be "Replicate", "Macro", or "Micro"
  workingPath = getwd(),    # The working directory
  dirName = dataType,       #@ Directory to use in case one doesn't want to use the default directory
  prefix = switch(dataType, ReplicateData = "replicate", MicroEvaluation = "micro", MacroEvaluation = "macro"),
  method = getEctdDataMethod()
)
{
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # Fri Jun 21 16:51 BST 2007 @445 /Internet Time/
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Returns a string containing the full path of the file which has a specified entry (1-9999) 
  # of specified data type (Replicate, Micro or Macro)
  # KEYWORDS:misc, IO 
  ###############################################################################  
  dataType <- match.arg(dataType)
  if (any(dataNumber < 1 || dataNumber > 9999)) ectdStop("dataNumber out of range, must be between 1 and 9999")
  # build the correct file name
  fileName <- switch(method, 
		  "CSV" = sprintf("%s%04d.csv", prefix, dataNumber ),
		  "RData" = sprintf("%s%04d.RData", prefix, dataNumber ),
		  ectdStop("Full file path only applicable for 'CSV' and 'RData' methods")
	  )
  fullPath <- file.path(workingPath, dirName, fileName)
  return(fullPath) 
}    

.readAndCheckInputFile <- function(
   file,     #@ the file to check
   variables =  NULL  #@ the variables that should be in the file
){
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # .readAndCheckInputFile.R Thu Jun 07 14:54:45 BST 2007 @621 /Internet Time/
  #
  # Author: Romain    
  ###############################################################################
  # DESCRIPTION: check that a csv file is valid ( exists, right format and 
  #              contains a given set of variables )
  # KEYWORDS: component:support io
  ###############################################################################
  
  if( !file.exists(file) ){
    ectdStop( "The file `$file` does not exist"   ) 
  }
    
  ## reads the first two lines of the file
  rl  <- readLines( file, n = 2 )
  
  # search for NONMEM header
  if( length(grep("^TABLE NO\\.", rl[1]) ) ) { 
    # it is a NONMEM file
    skip <- 1
    sep  <- ""
  } else{
    # work out if it is space delimited or comma delimited
    skip <- 0
    line2 <- rl[2] %-~% .quotes
    line2 <- gsub( "[[:space:]]*,[[:space:]]*", ",", line2 )
    line2 <- gsub( "[[:space:]]+", " ", line2 )
    line2 <- gsub( "^[[:space:]]", "", line2 )
    line2 <- gsub( "[[:space:]]$", "", line2 )
    
    gx.sp <- gregexpr( "[[:space:]]", line2)[[1]]
    gx.sp <- gx.sp[ gx.sp > 0 ]
    gx.co <- gregexpr( ",", line2)[[1]]
    gx.co <- gx.sp[ gx.co > 0 ]
    sep <- if( length(gx.sp) > length(gx.co) ) "" else ","
    
  }
  
  ## check the number of fields in the file
  nFields <- count.fields( file, skip = skip+1, sep = sep)
  if( !all(diff(nFields) == 0) ) ectdStop("The file does not have the same number of fields all along")
  
	## try to import the data
	out <- try( read.table(file, header = TRUE, skip = skip, sep = sep), silent = TRUE )
	if(class(out) == "try-error") ectdStop("error when importing the data in the file `$file`\n\t$out" )
  
  # Check for required variables in the import data
  .checkVariableNames(out, variables)
}

# Read from and write to an RData file
.writeToRData <- function(data, file, append = FALSE) {
	if (append & file.exists(file)) {
		data <- rbind(data, .readFromRData(file))
		.writeToRData(data, file, append = FALSE)
	}
	trySave <- try(save(data, file = file))
	file.exists(file) & class(trySave) != "try-error"
}

.readFromRData <- function(file, variables = NULL) {
	if (!file.exists(file)) ectdStop(paste("File '", file, "' does not exist", sep=""))
	x <- try(local(get(load(file))))
	if (class(x) == "try-error") ectdStop(paste("Could not import file '", file, "'", sep=""))
	.checkVariableNames(x, variables)
}

.checkVariableNames <- function(out, variables) {
	headers <- names(out)
	if( !is.null(variables)) {
		if(variables %!allin%  headers ) {
			checkCaseIssue <- casefold(variables) %!allin% casefold(headers)
			if (checkCaseIssue) ectdStop("The data file `$file` does not contain all the requested variables : "%.nt%
								"requested variables :  " %.% paste( '`' %.% variables %.% '`', collapse = ", " ) %.nt% 
								"header of the file  :  " %.% paste( '`' %.% headers %.% '`', collapse = ", " ) %.nt%
								"Diff                :  " %.% paste( '`' %.% variables[ variables %!in% headers ] %.%'`' , collapse = ", " )   )
			headers[match(casefold(variables), casefold(headers))] <- variables
		}
	}
	validNames( headers )
	out
}
