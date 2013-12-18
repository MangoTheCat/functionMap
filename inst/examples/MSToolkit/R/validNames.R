validNames <- function( ... ) { 
  ################################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # validNames.R Fri Jun 01 14:06:41 BST 2007 @587 /Internet Time/
  #
  # Author: Romain
  ################################################################################
  # DESCRIPTION: check if the names are valid R names
  # KEYWORDS: component:support check
  ################################################################################
  
  sapply( list(...), function(x){
    if( !is.null(x) && x %!~% "^[\\.]?[a-zA-Z][\\.0-9a-zA-Z]*$"  ){
	   ..nm <- get("..nm")
       wrongs <- paste( x[..nm], collapse= ", ")
       ectdStop( "$wrongs : invalid R name(s)" )
    }
  })
  TRUE
          
}

"checkColNames" <- function(dNames, cols) {
	validNames(cols)		# Check column names are valid inputs
	colTest <- cols %in% dNames
	if (!all(colTest)) {
		missCols <- paste("\"", cols[!colTest], "\"", sep="", collapse=", ")
		ectdStop(paste("Some required columns missing from the data:", missCols))
	}
	TRUE
}

.checkLogical <- function(..., single = TRUE) {
	inList <- list(...)
	isOk <- sapply(inList, is.logical)
	if (single) isOk <- isOk & sapply(inList, length) == 1
	if (any(myTest <- !isOk)) {
		theCall <- match.call(expand.dots = TRUE)
		theCall <- as.character(theCall)[1+1:length(inList)]
		whichNot <- theCall[myTest]
		errorMessage <- paste(if (single) "Single character" else "Character", "expected:")
		ectdStop(paste(errorMessage, paste(whichNot, sep=", ")))
	}
	isOk
}

.checkNumeric <- function(..., single = TRUE) {
	inList <- list(...)
	isOk <- sapply(inList, is.numeric)
	if (single) isOk <- isOk & sapply(inList, length) == 1
	if (any(myTest <- !isOk)) {
		theCall <- match.call(expand.dots = TRUE)
		theCall <- as.character(theCall)[1+1:length(inList)]
		whichNot <- theCall[myTest]
		errorMessage <- paste(if (single) "Single character" else "Character", "expected:")
		ectdStop(paste(errorMessage, paste(whichNot, sep=", ")))
	}
	isOk
}

.checkCharacter <- function(..., single = TRUE) {
	inList <- list(...)
	isOk <- sapply(inList, is.character)
	if (single) isOk <- isOk & sapply(inList, length) == 1
	if (any(myTest <- !isOk)) {
		theCall <- match.call(expand.dots = TRUE)
		theCall <- as.character(theCall)[1+1:length(inList)]
		whichNot <- theCall[myTest]
		errorMessage <- paste(if (single) "Single character" else "Character", "expected:")
		ectdStop(paste(errorMessage, paste(whichNot, sep=", ")))
	}
	isOk
}
