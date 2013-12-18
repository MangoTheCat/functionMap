parseCovMatrix <- function( 
   values,        #@ values used
   nCov,          #@ number of covariates
   tol = 1e-06
){
 	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# parseCovMatrix.R Fri Fri Jun 01 11:16:03 BST 2007 @469 /Internet Time/
	#
	# Author: Romain
	###############################################################################
	# DESCRIPTION: parses a covarariance matrix
  # KEYWORDS: check, component:support
	###############################################################################
   
  
   ### 1st step build the matrix
   if( is.matrix(values)){
     mat <- values
   } else {
     values <- parseCharInput( values, sort = FALSE )
     if(nCov == 1){
       length(values) == 1 || ectdStop("Dimension problem")
       mat <- matrix( values[1], nrow = 1, ncol = 1) 
     } else {
       if(length(values) == 1) values <- rep(values, nCov)
       nValues <- length(values) 
       if( nValues == nCov){
         mat <- diag( values) 
       } else if( nValues == (nCov)*(nCov+1)/2 ){
         mat <- matrix( 0, ncol = nCov, nrow = nCov)
         mat[ upper.tri(mat, diag = TRUE) ] <- values
         mat <- t(mat)
         mat[ upper.tri(mat, diag = TRUE) ] <- values
       } else{
         ectdStop("Dimension Problem") 
       }
     }
   } 
   
   # 2nd step, ensure the matrix is positive definite 
   checkSymmetricPDMatrix( mat, tol )
   
   mat    
       
}
               
