convertToSASCode <- function(
  code    #@ matrix of 3 columns describing the changes to make to the data
){
  ###############################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # convertToSASCode.R Wed Jun 27 12:46:29 BST 2007 @532 /Internet Time/
  #
  # Author: Romain, Rich    
  ###############################################################################
  # DESCRIPTION: convert dataChanges to SAS code
  # KEYWORDS: component:analysis
  ###############################################################################
  # TESTME
  
  # Parse inputs
  if (!length(code)) return("")
  if (!is.matrix(code) & !is.vector(code)) ectdStop("Unrecognised format for SAS Code conversion")
  if (is.matrix(code) && ncol(code) != 3) ectdStop("Matrix to convert to SAS Code must have 3 columns")
  # <TODO>
  # <=, >=, <, >
  repSas <- cbind(c("==", "!=", "\\&", "\\|"), c("EQ", "NE", "AND", "OR"))  # Replace strings for SAS syntax
  # </TODO>
  
  # Data changes if a matrix has been provided
  sasCode <- if (is.matrix(code)) {
     for (i in 1:nrow(repSas)) code[,1] <- gsub(repSas[i,1], repSas[i,2], code[,1])
     paste("IF", code[,1], "THEN", code[,2], "=", code[,3], ";", collapse="")  
  }
  else {
     for (i in 1:nrow(repSas)) code <- gsub(repSas[i,1], repSas[i,2], code)
     paste("IF", code, ";", collapse="")
  }
  sasCode
}
