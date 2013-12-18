
.recurseIdx <- function(vidx, netm) {
	tmp <- unique(c(vidx, unique(as.vector(unlist(sapply(vidx, function(x) which(netm[x, ] != 0)))))))
	if (length(vidx) == length(tmp)) {
		returnthis <- tmp
	} else {
		returnthis <- .recurseIdx(tmp, netm)
	}
	return(returnthis)
}

