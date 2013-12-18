checkSymmetricPDMatrix <- function( mat, tol = 1e-6 ){
 	###############################################################################
	# � Mango Solutions, Chippenham SN14 0SQ 2006
	# checkSymmetricPDMatrix.R Fri Jun 01 13:13:38 BST 2007 @551 /Internet Time/
	#
	# Author: Romain    
	###############################################################################
	# DESCRIPTION: checks if a matrix is symmetric and positive definite
  # KEYWORDS: component:support
	###############################################################################
  
  if(any(is.na(mat))) 
    ectdStop("Missing values not allowed in covariance matrix")

  if( diff(dim(mat)) != 0) 
    ectdStop("matrix not square")

  if( any( mat != t(mat) ) ) 
    ectdStop( "matrix not symmetric" )
  
  ev <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
    
  # This test for positive-definiteness was found in the function mvrnorm
  if (!all(ev >= -tol * abs(ev[1]))) 
      ectdStop("matrix not positive definite")
  
}

