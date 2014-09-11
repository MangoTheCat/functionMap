
##' Create the network object for the map of functions
##'
##' If return a matrix, the row means the functions be invoked by the function of rowname, 1 means 'invoked'. 
##' 
##' @title Create the network object for the map of functions
##' @param funlist A list of functiions structure. The result of the function \code{\link{parseRfolder}}.
##' @param omitpattern An regular expression. The function names which match the regular expression will be omitted.
##' @param rootfunc A function name which will be used as root node. Only it's children will be returned.
##' @param transpose Whether to transpose the matrix of network structure.
##' @param returnmatrix Whether to return a matrix rather than the default network object.
##' @return A network object or matrix.
##' @author Mango Solutions
##' @examples \dontrun{
##' lfun <- parseRfolder(system.file("examples", "R", package = "functionMap"))
##' createNetwork(lfun)
##' }
##' 

createNetwork <- function(funlist, omitpattern = "^\\.|%", rootfunc = "", transpose = FALSE, returnmatrix = FALSE) {
	fun.names <- names(funlist)
	pfun.user <- lapply(funlist, function(x) intersect(x, fun.names))
	netm <- matrix(0, length(fun.names), length(fun.names), dimnames=list(fun.names, fun.names))
	for(i in 1:length(fun.names)) {
		if(length(pfun.user[[i]]) != 0) {
			for(j in 1:length(pfun.user[[i]])) {
				netm[i, which(fun.names == pfun.user[[i]][j])] <- 1
			}
		}
	}
	diag(netm) <- 0 
	
	omitFun <- fun.names[grep(omitpattern, fun.names)]
	if(length(omitFun) > 0) {
		idx <- as.vector(sapply( omitFun, function(x) which(fun.names == x)))
		if (length(idx) > 0) {
			netm <- netm[- idx, - idx]
		}
	}
	
	rootfunc <- rootfunc[rootfunc %in% fun.names]	
	if(length(rootfunc) > 0) {
		if (length(rootfunc) > 1) {
			rootfunc <- rootfunc[1]
			warning("Only the 1st 'root' function was used!")
		}
		idx0 <- .recurseIdx(which(rownames(netm) == rootfunc), netm)
		if (length(idx0) > 0) {
			netm <- netm[idx0, idx0]
		}
	}
	
	if (transpose) netm <- t(netm)
	if (returnmatrix) return(netm)
	netg <- network(netm)
	return(netg)
}





