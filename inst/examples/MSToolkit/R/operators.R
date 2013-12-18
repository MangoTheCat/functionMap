
# TRUE if no element of x matches the regex rx
`%!~%` <- function(x, rx){
  out <- any( rxout <- regexpr(rx, x) == -1 )
  assign( "..nm", which(rxout), parent.frame() )
  out
}

# negation of %in%
`%!in%`    <- function(...) !`%in%`(...)

# wrap all around %in%
`%allin%`  <- function(...) all( `%in%`(...) )
`%!allin%` <- function(...) !all( `%in%`(...) )

# string concatenation
`%.%` <- function(s1, s2) {
  paste(s1, s2, sep = "") 
}                                  

# .. with a newline
`%.n%` <- function(s1, s2) {
  paste(s1, s2, sep = "\n", collapse = "\n") 
}

# .. with a new line and a tabulation
`%.nt%` <- `%.tn%` <- function(s1, s2) {
  paste(s1, s2, sep = "\n\t") 
}

# .. remove a pattern from a string
`%-~%` <- function(txt, rx){
  gsub( rx, '', txt )                
}     

# assign value to the value of name
`%<-%` <- function(name, value){
  assign( name, value, parent.frame() )
}
