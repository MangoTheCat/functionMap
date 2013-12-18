createEmptyMicro <- function(
  doses, # A numeric vector containing the DOSES column of the micro data
  doseCol = "DOSE", # The name of the dose column in the data frame
  microColumnNames =  c("doseName" = doseCol, "MEAN", "SE", "LOWER", "UPPER", "N")  # The name of the data frame's columns
)
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # createEmptyMicro.R Wed Jun 27 14:31:26 BST 2007 @445 /Internet Time/
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION:  Creates a micro evaluation dataframe whose DOSE column is given by an argument and which
  # has the value NA for the rest of its entries.
  # KEYWORDS: 
  # Documented in Support Functions Design Specification 
  ###############################################################################
{
  doses <- parseCharInput( doses )
  if( length(doses) == 0 || is.null(doses) )
    ectdStop("Empty or null doses vector")

  # Check for duplicate doses and disallow
  if(any(duplicated(doses)))
    ectdStop("duplicated doses in argument")
  
  # The names of the columns must include the string "doseCol"
  if(doseCol %!in% microColumnNames)
  	ectdStop(paste(doseCol, "does not occur in the microColumnNames"))
 
  result <- as.data.frame(matrix(nrow = length(doses), ncol = length(microColumnNames)))
  names(result) <- microColumnNames
  result[doseCol] <- doses
  result
}                                  