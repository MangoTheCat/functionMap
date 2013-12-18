parseRangeCode <- function( 
  code      #@ code to parse
){
  ###############################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # parseRangeCode.R Thu Jun 21 11:52:00 BST 2007 @494 /Internet Time/
  #
  # Author: Romain    
  ###############################################################################
  # DESCRIPTION: parse R code that represents range
  # KEYWORDS: component:support 
  ###############################################################################
  if(missing(code) || is.null(code) ) return(NULL)
  
  ## handle the if
  if( any( regexpr("\\|", code)  > 0 )  )
    ectdStop( "`|` not allowed in the range code" )
  
  code <- unlist( strsplit( code, "[;&,]" ) )
 
  ## handle multiple <>  
  out <- NULL
  gx  <- gregexpr( "[<>]=?", code  )
  for( i in seq(along = code)){
    gxi <- gx[[i]]
    if(length(gxi) == 1 && gxi == -1){
      ectdStop("No comparator in the code : <, >, >=, <=") 
    }    
    if( length(gxi) > 2 ){ 
      ectdStop("Too many comparators (<, >, >=, <=) in the range code")
    }
   
    out <- switch( length(gxi), 
      c( out, code[i]), 
      c( out, 
        substring(code[i], 1, gxi[2]-1 ), 
        substring(code[i], gxi[1] + attr(gxi, "match.length")[1] )) ) 
  }
  
  ## paste and parse the code
  out <- paste( "(", out , ")", sep = "", collapse = "&") %-~% "[[:space:]]" 
  result <- try( parse( text = out ),  silent = TRUE )
  if( class(result) == "try-error" ) {
    ectdStop('parsing problem: ' %.% ( result %-~% "^[^:]*:" )     ) 
  }  
  result 
  
}              

