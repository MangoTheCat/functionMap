initialChar <- function( 
  txt,                                    # text to parse 
  adm = "[:lower:]",                      # acceptable values for the output
  err = "Not acceptable value"            # error message if not in the acceptable values
  ){
 	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# initialChar.R Fri Sun Jun 03 20:05:15 BST 2007 @836 /Internet Time/
	#
	# Author: Romain
	###############################################################################
	# DESCRIPTION: gets the first letter from a character string and "lower case" it
  # KEYWORDS: check, component:support
	###############################################################################
  
  ### tidy up the input
  if(missing(txt) || is.null(txt) || length(txt) == 0) 
    ectdStop("no character string to use")
  if(length(txt) > 1) ectdWarning("only the first element has been used")
  txt <- as.character( txt[1] )  
  
  ### figure out where is the first letter in that string and extract it
  re <- regexpr("[[:alpha:]]", txt)
  if( re == -1 ) ectdStop("No character in the string")
  out <- casefold( substring(txt,re,re ) )
  
  ### checks if the output is one of the possibilities given
  rx <- paste("[", casefold(adm) , "]", sep = "")
  if( out %!~% rx ) ectdStop( err )
  
  out
}

