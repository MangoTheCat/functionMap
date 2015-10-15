
##' Create the network object for the map of functions
##'
##' If return a matrix, the row means the functions be invoked by the function of rowname, 1 means 'invoked'. 
##' 
##' @title Create the network object for the map of functions
##' @param funlist A list of functiions structure. The result of the function \code{\link{parseRfolder}}.
##' @param omitpattern An regular expression. The function names which match the regular expression will be omitted.
##' @param rootfunc A function name which will be used as root node. Only it's children will be returned. NULL for the whole (might be disconnected) network
##' @param transpose Whether to transpose the matrix of network structure.
##' @param returnmatrix Whether to return a matrix rather than the default network object.
##' @return A network object or matrix.
##' @author Mango Solutions
##' @importFrom network network
##' @examples \dontrun{
##' lfun <- parseRfolder(system.file("examples", "R", package = "functionMap"))
##' createNetwork(lfun)
##' }
##' 

createNetwork <- function(funlist, omitpattern = "^\\.|%", rootfunc = NULL, transpose = FALSE, returnmatrix = FALSE) {
    if (any(duplicated(names(funlist)))) {
        warning('There are duplicated names in the input funlist. This is probably caused by multiple definition of a single function. We will merge the same name entry together in the following processing.')
        funlist <- tapply(funlist, names(funlist), function(x) unlist(unname(x)))
    }
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
			netm <- netm[- idx, - idx, drop=FALSE]
		}
	}
	
    if (!is.null(rootfunc)) {
        rootfunc0 <- rootfunc
        rootfunc <- rootfunc[rootfunc %in% fun.names]	
        if(length(rootfunc) > 0) {
            if (length(rootfunc) > 1) {
                rootfunc <- rootfunc[1]
                warning("Only the 1st 'root' function was used!")
            }
            idx0 <- .recurseIdx(which(rownames(netm) == rootfunc), netm)
            if (length(idx0) > 0) {
                netm <- netm[idx0, idx0, drop=FALSE]
            }
        } else {
            stop(sprintf(
                "The supplied rootfunc: %s is not existing in the funlist!", 
                paste(rootfunc0,collapse=',')))
        }
    }
	
	if (transpose) netm <- t(netm)
	if (returnmatrix) return(netm)
	netg <- network(netm)
	return(netg)
}





