parseHashString <- function(
  input, #@ hash and comma separated character vector to split into numeric values 
  ...,   #@ further arguments to pass to parseCharInput
  missingMsg
  ){
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # parseHashString.R Thu Jun 07 10:03:11 BST 2007 @418 /Internet Time/
  #
  # Author: Romain
  ###############################################################################
  # DESCRIPTION: parses a hash and comma separated string.
  # KEYWORDS: component:support
  ###############################################################################
                
 .requiredArgs(input, missingMsg)
 
  ## check if there is anything to do   
  if( is.null(input) ) return(input) 
  if( !is.list(input) && any( regexpr("\\#", input)  > 0 )  ) {
    input <- as.list( unlist( strsplit(input, "\\#+") ) )
  }
  out <-  lapply( input, parseCharInput, ... )
      
  out                             
}

