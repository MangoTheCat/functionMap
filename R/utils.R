
in_list <- function(elem, list) {
  for (e in list) if (identical(elem, e)) return(TRUE)
  FALSE
}

match_list <- function(elem, list) {
  for (i in seq_along(list)) if (identical(elem, list[[i]])) return(i)
  NA_integer_
}

#' Drop NULL elements from a list

drop_null <- function(x) {
  Filter(function(xx) !is.null(xx), x)
}

.recurseIdx <- function(vidx, netm) {
	tmp <- unique(c(vidx, unique(as.vector(unlist(sapply(vidx, function(x) which(netm[x, ] != 0)))))))
	if (length(vidx) == length(tmp)) {
		returnthis <- tmp
	} else {
		returnthis <- .recurseIdx(tmp, netm)
	}
	return(returnthis)
}

.matchPos.x <- function(x){
	X <- tryCatch({
				as.numeric(x)
			},warning = function(w){
				match.arg(x,c("center", "left", "right"))
			})
	return(X)
}

.matchPos.y <- function(y){
	Y <- tryCatch({
				as.numeric(y)
			},warning = function(w){
				match.arg(y,c("bottom", "center", "top"))
			})
	return(Y)
}

.tempId <- function(){
	id = paste('ID', format(Sys.time(), "%Y%m%d%H%M%S"), proc.time()[3]*100, sep="_")
	return(id)
}

.Trim.too.long <- function(x, width=60) {
    # trim too long line
    if(length(ind<-which(nchar(x)>width))){
        x[ind] <- 'Very long expression'#paste(substring(x[ind], 1, width-3), '...')
    }
    x
}
