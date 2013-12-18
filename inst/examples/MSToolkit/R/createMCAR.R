"createMCAR" <- function(
	data,                             #@ Data Structure to which to add missing flag
	prop = 0,                         #@ Proportion of observations to set to missing
	rule,                             #@ Rule to specify which observations of the data shold be included
	seed = .deriveFromMasterSeed( ),  #@ Random seed
	flagName = getEctdColName("Missing")
){
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# createMCAR.R Fri Jun 08 08:56:54 BST 2007 @372 /Internet Time/
	#
	# Author: Romain    
	###############################################################################
	# DESCRIPTION: missing completely at random flags
	# KEYWORDS: component:data:missingness
	###############################################################################

	set.seed( seed )
	validNames( flagName )
  
	## handle correct values of `prop`
	if( prop < 0 || prop > 1 ) ectdStop( "`prop` must be within range [0,100]" )  
	if( prop == 0 ){
		if( !(flagName %in% colnames(data)) ) data[[flagName]] <- rep(0, nrow(data))
		return( data ) # don't have to care about the rule in that case
  }
  
	## check if the existing missing flag
	if( flagName %in% colnames(data) && !all(data[[flagName]] %in% 0:1)  ) {
		ectdStop( "The missing flag ($flagName) does not only contain 0 and 1" )
	}
  
	nr <- nrow(data)
  
	if( missing(rule)){ 
		include <- rep(TRUE, nr )    
	} 
	else {
    	rule <- parseRCode( rule )
    	include <- try( eval( rule, data ) , silent = TRUE)
    	## check if something went wrong when executing the code
    	if( class(include) == "try-error" ) {
    		ectdStop( "The rule is not correct R code : " %.nt% ( include %-~% "^[^:]*:"  ) )
		}
    
		## checks on the output of the rule
    	if( !is.logical(include) ) ectdStop("The `rule` does not produce a logical vector") 
		if( length(include) != nrow(data) ){
    		ectdStop(
        		"Dimension problem with `rule` code" %.n% 
        		"\trows in the data: $nr, length of logical vector: " %.% length(include)  ) 
		}
    
		if( ! any( include ) ) ectdStop( "All logical values are false"  )
	} 
  
	### sample the missing values
	missingSample <- sample( c(1, 0), prob = c( prop, 1 - prop), size = sum(include), replace = TRUE )
  
	### create the new missing flag  
  	missingFlag <- replace( rep(0, nrow(data)), include, missingSample )
    
	### add or modify the missing flag in the data set  
	data[[ flagName ]] <- if( flagName %in% colnames(data) ){
		1 * ( data[[ flagName ]] | missingFlag ) 
	} else missingFlag
  data 
  
}
