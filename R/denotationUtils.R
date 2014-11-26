
## uitls for create attributes for network according to 
## S3, S4 or other code properties

#' guess.s3.from.names
#'
#' With provided knowlege of possible S3 generic names, return a data.frame describing the category of a vector of function names
#' Typically used when the target is not a package, do not have a NAMESPACE
#' 
#' @param n network of just a vector of names
#' @param knownS3generic names of known S3 generic methods
#' @return data.frame describing the vertices
#' @export
guess.s3.from.names <- function(n, knownS3generic=tools:::.get_S3_generics_as_seen_from_package('',F)) {
    if (is.network(n)) {
        vn <- network.vertex.names(n)
    } else {
        vn <- n
    }
    x <- Filter(function(x) length(x)>0, sapply(knownS3generic, function(pat) which(substring(vn,1,nchar(pat)+1)==paste(pat,'.',sep=''))))
    re <- data.frame(node=vn, category='function', generic=NA, stringsAsFactors=FALSE)
    for(i in names(x)) {
        re[x[[i]], 'category'] <- 'S3'
        re[x[[i]], 'generic'] <- i
    }
    re
}

#' guess.s3.from.dir
#'
#' scan a package folder to find if there is any S3 methods
#'
#' It's basically difficult task to determine if a function is S3.
#' There are two situations can be discovered by this function.
#' 1) When the given path is a package, hence the S3method definition are available in the NAMESPACE
#' 2) Some new registered S3 generic method which defined using \code{UseMethod}
#'
#' @param base.path the path which we want to check
#' @return data.frame of S3 methods
#' @export
guess.s3.from.dir <- function(base.path) {
    re <- NULL
    if (file.exists(file.path(base.path,'NAMESPACE'))) {
        L <- Filter(function(x) x[[1]]=='S3method', as.list(parse(file.path(base.path,'NAMESPACE'))))
        if (length(L)){
            re <- do.call('rbind', lapply(L, function(x) as.character(x[2:3])))
            colnames(re) <- c('method', 'object.class')
        }
        return( re )
    }
    # if NAMESPACE is not available, scan all .R files for defined S3 methods (check UseMethods Pattern)
    L <- list()
    for (fn in list.files( base.path, pattern='\\.[R|r]$', rec=TRUE, full=TRUE)) {
        L <- c(L, Filter(function(x) 
                    (x[[1]]=='<-' || x[[1]]=='=') && 
                    (is.call(x[[3]]) && x[[c(3,1)]]=='function') &&
                     (x[[c(3,3,1)]]=='UseMethod' ||
                     (x[[c(3,3,1)]]=='{' && is.call(x[[c(3,3,2)]]) && x[[c(3,3,2,1)]]=='UseMethod') || 
                     (is.call(x[[c(3,3,1)]]) && x[[c(3,3,1,1)]]=='UseMethod'))
                    , as.list(parse(fn))))
    }
    if (length(L)) {
        re <- do.call('rbind', lapply(L, function(x) c(as.character(x[[2]]), names(x[[c(3,2)]])[1])))
        colnames(re) <- c('method', 'object.class')
    } 
    re
}

