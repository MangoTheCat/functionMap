"parseRCode" <- function( 
	code     #@ code to parse
){
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# parseRCode.R Thu Jun 07 14:39:44 BST 2007 @610 /Internet Time/
	#
	# Author: Romain/Rich P
	###############################################################################
	# DESCRIPTION: parse R code
	# KEYWORDS: component:support 
	###############################################################################
	result <- try( parse( text = code ),  silent = TRUE )
	if( class(result) == "try-error" ) ectdStop(paste('parsing problem:', ( result %-~% "^[^:]*:" ), sep="")) 
	result
}
