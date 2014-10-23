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


#' dfs
#' 
#' dfs travel graph in matrix representation
#' @param n adj matrix
#' @param v index from which we start, missing then searhing all connected component
#' @param direction forward, backward or bidirection
#' @param return traveled vertexes indices or list of connected components (missing v)
#' @export
dfs.matrix.travel <- function(n, v, direction='forward') {
    if (prod(dim(n))==0) return(NULL)
    traveled <- new.env(parent=emptyenv())
    cc.index <- 1

    dfs0 <- function(u) {
        next.u <- switch(direction, 
            forward  = which(n[u,] > 0),
            backward = which(n[,u] > 0),
            bidirection = which(n[u,]>0|n[,u]>0))

        for(i in next.u) {
            if (exists(as.character(i),env=traveled) && get(as.character(i),env=traveled)>0) next
            assign(as.character(i), cc.index, env=traveled)
            dfs0(i)
        }
    }

    if (!missing(v)) {
        assign(as.character(v),cc.index, env=traveled)
        dfs0(v)
        re <- as.integer(ls(traveled))
        return(sort(re))
    }
    # connected components
    L <- list()
    for(v in 1:NROW(n)) {
        if (exists(as.character(v), env=traveled) && get(as.character(v),env=traveled)) next
        assign(as.character(v),cc.index, env=traveled)
        dfs0(v)
        re <- unlist(eapply(traveled, function(x) x==cc.index))
        L[[cc.index]] <- as.integer(names(re)[re])
        cc.index <- cc.index + 1
    }
    L
}

