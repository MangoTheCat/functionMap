# Other useful network utils

#' all.isolated
#'
#' Test if all vertexes are isolated
#' @param n network object
#' @return logical
#' @export
all.isolated <- function(n) {
    stopifnot(is(n, 'network'))
    all(sapply(n$iel, length) + sapply(n$oel, length) == 0)
}

#' isolated.vertexes
#' 
#' return isolated.vertexes of a network object
#' @param n network object
#' @return isolated vertexes names
#' @export
isolated.vertexes <- function(n, need.plot=FALSE) {
    stopifnot(is(n, 'network'))
    nms <- sapply(n$val, el, 'vertex.names')
    in.degree <- sapply(n$iel, length)
    out.degree <- sapply(n$oel, length)
    ind <- which(in.degree + out.degree == 0)
    re <- nms[ind]
    if (need.plot) {
        if (length(re)>0) {  
           plotFunctionMap(network(n[re,re]),TRUE)
        } else {
           warning('No isolated vertexes found.')
        }
    }
    re
}


