
add <- function(x, y) {
	x + y 
}


fib <- function(n) {
    
    if (length(n) != 1) { stop("n must be length 1") }
    
    n <- as.integer(n)[1]
    
    if (is.na(n)) { stop("n not recognized") }
    
    if (n < 1) { stop("n should be an integer greater than 0") }
    
    x <- rep(1, n)
    
    if (n > 2) { 
		for(i in seq.int(from = 3, to = n))  { 
			x[i] <- add(x[i - 1], x[i - 2]) 
		} 
	}
    
    return(x)
}
