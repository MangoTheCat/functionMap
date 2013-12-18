createInterims <- function( 
  subjects,                          #@ set of subject to be assigned to interim
  proportion,                        #@ Proportion of total subjects per interim
  seed = .deriveFromMasterSeed(),    #@ random seed to use
  idCol = getEctdColName("Subject"),                      #@ name of the ID column
  interimCol = getEctdColName("Interim"),            #@ name of the imterim column
  method = "Sample"                  #@ the method to use
){
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # createContinuousCovariates.R Fri Jun 01 10:41:35 BST 2007 @445 /Internet Time/
  #
  # Author: Romain
  ###############################################################################
  # DESCRIPTION: create the interim data
  # KEYWORDS: datagen, component:data:interim
  ###############################################################################
  
  set.seed(seed)
          
  ## validate the names
  validNames( idCol, interimCol)
  if( idCol == interimCol ){
    ectdStop("`idCol` and `interimCol` should be different")
  }
  
  ## tidy up the method argument
  method <- initialChar(method, "ps", "method must be `Sample` or `Proportion`")
                        
  ## handle the case where sujects is of length 1
  subjects <- .expandSubjects( subjects ) 
  nSubjects <- get("nSubjects")
  
  ## generate the non-cumulative proportions from the cumulative
  proportion <- if( missing(proportion) || is.null(proportion) ) {
    1 
  } else {
    .nonCumulativeFromCumulative( proportion )
  }
  interimValues <- seq( along = proportion)
  
  ## generate the interim code
  interim <- switch(method, 
    "s" = {    # Sample method
      sample(interimValues, size = nSubjects, prob = proportion, replace = TRUE)
    },
    "p" = {    # Proportion method
       sizes <- floor( nSubjects * proportion )
       interim <- c( rep( interimValues, sizes ), 
         sample(interimValues, prob = proportion, size = nSubjects - sum(sizes) ) )
       interim <- sample( interim )
     })
  
  .eval( "data.frame( $idCol = subjects, $interimCol = interim)" )
  
}

