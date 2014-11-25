
## uitls for create attributes for network according to 
## S3, S4 or other code properties

#' guess.s3methos.from.names
#' 
#' @param n network of just a vector of names
#' @return data.frame describing the vertices
#' @export
guess.s3methos.from.names <- function(n) {
    if (is.network(n)) {
        vn <- network.vertex.names(n)
    } else {
        vn <- n
    }
    x <- Filter(function(x) length(x)>0, sapply(names(.knownS3Generics), function(pat) which(substring(vn,1,nchar(pat)+1)==paste(pat,'.',sep=''))))
    re <- data.frame(node=vn, category='function', generic=NA, stringsAsFactors=FALSE)
    for(i in names(x)) {
        re[x[[i]], 'category'] <- 'S3'
        re[x[[i]], 'generic'] <- i
    }
    re
}


