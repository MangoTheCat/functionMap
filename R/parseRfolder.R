
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
#' @export
#' @examples \dontrun{
#'   n1 <- network.from.rpackage('MASS')
#'   (n1 %e% 'weights')[1:10]
#'   plot(eForce(n1))
#'   plot(eForce(n1, use.network.attr=TRUE ))
#'   plot(eForce(n1 %s% which( n1%v% 'category' !='outpackage'), use.network.attr=TRUE ))
#' }

network.from.rpackage <- function(base.path,  rfilepattern = "\\.[R|r]$") {
    S3 <- guess.s3.from.dir(base.path)
    rs <- list.files(file.path(base.path,'R'), pattern=rfilepattern, rec=TRUE, full=TRUE)
    v.names <- c()
    L <- list()
    # There might be possible duplicated definition of single function, we merge them all
    for(fn in rs) {
        x <- try(parseRscript(fn), silent=TRUE)
        if (is(x,'try-error')) next
        for(v in names(x)) {
            ind <- match(v, v.names)
            if (is.na(ind)) {
                v.names <- c(v.names, v)
                ind <- length(v.names)
                L[[ind]] <- x[[v]]
            } else {
                L[[ind]] <- c(L[[ind]], x[[v]])
            }
        }
    }
    names(L) <- v.names
    ## v.out is the vertex not defined in this pacakge
    v.out <- setdiff(unlist(L), v.names)
    ## v.s3.out is the S3 methods appearing in v.out, we can't determine if it's defined in or not in this pacakge, because we 
    ## don't know the runtime information of the object of the call
    v.s3.out <- v.out[which(v.out %in% S3[,1])]
    ##
    elist <- do.call('rbind', lapply(v.names, function(x) {
        if (length(L[[x]])==0) return(NULL)
        z <- table(L[[x]])
        z.name <- names(z)
        data.frame(tails=x, heads=z.name, weights=as.vector(z), stringsAsFactors=FALSE)
    }) )
    net <- network(elist, matrix.type='edgelist', ignore.eval=FALSE)
    v.all <- network.vertex.names(net)
    net %v% 'category' <- ifelse(v.all %in% v.names, 'inpackage', 
                                       ifelse(v.all %in% v.s3.out, 'S3generic', 'outpackage'))
    ## v.out
    net
}

#' s4.source.from.rpackage
#'
#' Extract source code from S4 definitions from a R package
#' 
#' @param base.path path to the R source, assume it to be a package base path
#' @param rfilepattern some author may use extension ".[qQ][sS]" other than [Rr], this option can select from those
#' @return network object
s4.source.from.rpackage <- function(base.path, rfilepattern = '\\.[Rr]$'){
    # s4.list <- guess.s4.from.dir(base.path)
    # if (is.null(s4.list) || is.null(s4.list$S4.methods)) return(NULL)
    # If it's a installed package , parseS4fromNs will work well
    s4.defn <- try(dumpS4Generic(basename(base.path), style='S4'), silent=TRUE)
    # If not, we may have to extract.S4.defn from source code, install them in .GlobalEnv, and dump again
    if (is(s4.defn, 'try-error')) {
        txt <- try(extract.S4.defn(srclist= list.files(file.path(base.path,'R'), pattern=rfilepattern, full=TRUE, rec=TRUE)),
                silent=TRUE)
        if (is(txt, 'try-error')) {
            stop('Failed to extract S4 definition from this package!')
        }
        try(eval(parse(text=txt)), silent=TRUE)
        # if above is success, those definitions are exported to .GlobalEnv and we can use parseS4fromNs
        s4.defn <- try(dumpS4Generic(style='S4'), silent=TRUE)
        if (is(s4.defn, 'try-error')) {
            stop('Failed to extract S4 definition from this package!')
        }
    }
    s4.defn
}




