
##' Parse the R scripts in a folder and return the function structure
##'
##' @title Parse the R scripts in a folder and return the function structure
##' @param rpath The path of the R script folders or files.
##' @param rfilepattern An optional regular expression. Only file names which match the regular expression will be parsed.
##' @return A list of the functions structure. 
##' @author Mango Solutions
##' @examples \dontrun{
##' rpath <- system.file("examples", "R", package = "functionMap")
##' parseRfolder(rpath)
##' }
##' 

parseRfolder <- function(rpath, rfilepattern = "\\.[R|r]$", returnfilename = FALSE) {
	rfiles <- character(0)
	isdir <- file.info(rpath)$isdir
	for (i in 1:length(isdir)) {
		if (identical(isdir[i], TRUE)) rfiles <- c(rfiles, list.files(rpath[i], pattern = rfilepattern, full.names = TRUE))
		if (identical(isdir[i], FALSE)) rfiles <- c(rfiles, rpath[i])
	}
	
	if (length(rfiles) == 0) stop("There is no R script to be parsed!")
	
	tmp.func <- list()
	for (i in 1:length(rfiles)) {
		tmp.parse <- try(parseRscript(rfiles[[i]]), silent = TRUE)
		if (!inherits(tmp.parse, "try-error")) tmp.func[[i]] <- tmp.parse
	}
	if (returnfilename) {
		names(tmp.func) <- basename(rfiles)
	} else {
		tmp.func <- rev(do.call("c", tmp.func))
		tmp.func <- rev(tmp.func[unique(names(tmp.func))])
	}
	
	return(tmp.func)
}


#' network.from.rpackage
#'
#' Similar to \code{\link{parseRfolder}}, but differences.
#' 1. \code{parseRfolder} don't assume the path is a root path to a package, but \code{network.from.rpackage} does.
#' 2. \code{parseRfolder} returns a names list of called functions while this one return a network object which can have more attributes
#'
#' @param base.path path to the R source, assume it to be a package base path
#' @param rfilepattern some author may use extension ".[qQ][sS]" other than [Rr], this option can select from those
#' @return network object
#' @examples \dontrun{
#'   n1 <- network.from.rpackage('MASS')
#'   n1 %e% 'isS3'
#'   plot(eForce(n1))
#'   plot(eForce(n1, use.network.attr=TRUE ))
#' }

network.from.rpackage <- function(base.path,  rfilepattern = "\\.[R|r]$") {
    S3 <- guess.s3.from.dir(base.path)
    rs <- list.files(file.path(base.path,'R'), pattern=rfilepattern, rec=TRUE, full=TRUE)
    v.names <- c()
    L <- list()
    for(fn in rs) {
        x <- try(parseRscript(fn), silent=TRUE)
        if (is(x,'try-error')) next
        for(v in names(x)) {
            ind <- match(v, v.names)
            if (is.na(ind)) {
                v.names <- c(v.names, v)
                ind <- length(v.names)
                L[[ind]] <- unique(x[[v]])
            } else {
                L[[ind]] <- unique(c(L[[ind]], x[[v]]))
            }
        }
    }
    names(L) <- v.names
    s3.nms <- paste(S3[,1],S3[,2],sep='.')
    s3.ind <- match(v.names, s3.nms)
    s3.names <- match(s3.nms, v.names)
    ## now we can create network with considering the S3
    v.out <- setdiff(unlist(L), v.names)
    ## we drop those non-S3-call out, but only keep S3 call out
    v.s3.out <- v.out[which(v.out %in% S3[,1])]
    elist <- do.call('rbind', lapply(v.names, function(x) {
        z <- L[[x]]
        z <- z[ z %in% v.names | z %in% v.s3.out ]
        if (length(z)) {
            data.frame(tails=x, heads=z, isS3= z %in% v.s3.out, stringsAsFactors=FALSE)
        } else {
            NULL
        }
    }) )
    net <- network(elist, matrix.type='edgelist', ignore.eval=FALSE)
    net %v% 'category' <- ifelse(network.vertex.names(net) %in% v.names, 'inpackage', 'S3generic')
    net
}

