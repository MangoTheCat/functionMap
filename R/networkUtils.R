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


#' dfs.matrix.travel
#' 
#' Depth-First travel a graph in a matrix representation
#'
#' @param n adj matrix
#' @param v index from which we start, missing then searhing all connected component
#' @param direction forward, backward or bidirection
#' @return traveled vertexes indices from \code{v} or a list of connected components (missing \code{v})
#' @export
dfs.matrix.travel <- function(n, v, direction='forward') {
    if (is(n,'network')) n <- as.matrix(n)
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
            Recall(i)
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

#' topo.sort
#'
#' compressed topological sort
#' @param n network
#' @param direction which direction
#' @return order
topo.sort <- function(n, direction='forward') {
    nv <- if (is.network(n)) n$gal$n
          else NROW(n)
    explored <- new.env(parent=emptyenv())
    current_label <- 1
    f <- list()

    dfs0 <- function(u) {
        assign(as.character(u), TRUE, env=explored)
        next.u <- switch(direction, 
            forward  = which(n[u,] > 0),
            backward = which(n[,u] > 0))
        for(i in next.u) {
            if (exists(as.character(i), env=explored) && get(as.character(i), env=explored)>0) next
            assign(as.character(i), TRUE, env=explored)
            Recall(i)
        }
        f[[ as.character(u) ]] <<- current_label
        current_label <<- current_label + 1
    }

    for(from.v in 1:nv) {
        if (exists(as.character(from.v), env=explored) && get(as.character(from.v), env=explored)>0) next
        dfs0(from.v)
    }
    f <- unlist(f)
    ind <- as.integer(rev(names(f)))
    #
    V <- integer(nv)
    V[ind[1]] <- 1
    ii <- 1
    #
    while( (ii<-ii+1) <= nv ) {
        k <- which(n[ind[1:ii-1],ind[ii]] > 0)
        if (length(k)) {
            V[ind[ii]] <- max(V[ind[k]]) + 1
        } else {
            V[ind[ii]] <- 1
        }
    }
    V
}


#' page.rank
#'
#' @param net network object or just a matrix
#' @param beta taxation parameter
#' @param tol tolerance
#' @param topic.ind restric target teleport position
#' @return page rank vector
#' @export
#' @examples \dontrun{
#'      (r <- page.rank(net))
#'      page.rank(net, topic.ind = net %v% 'category' != 'outpackage')
#'      net %v% 'value' <- as.vector(r) * 5000
#'      plot(eForce(net, use.network.attr=TRUE))
#' }
page.rank <- function(net, beta=0.85, tol=1e-5, topic.ind) {
    # typically we want to score the complex function higher
    # So we set the transition matrix to net[,] i.e. from callee -> caller
    M <- unname(as.matrix(net))
    m.col.sum <- colSums(M)
    ind <- which(m.col.sum!=0)
    if (!length(ind)) {
        stop('All column sums to Zero!')
    }
    for(i in ind) {
        M[,i] <- M[,i]/m.col.sum[i]
    }
    N <- NCOL(M)
    v <- rep(1,N)/N
    if (missing(topic.ind)) {
        topic.ind <- 1:N
    }
    # e <- e/sum(e), should not normalize, have to keep t(1) %*% v == const
    lastv <- rep(0, N)
    while( max(abs(lastv - v)) > tol ) {
        lastv <- v
        v <- beta * M %*% v
        # avoid deadend or trap, assuming sum(v) == const == 1
        leaked <- 1-sum(v)
        v[topic.ind] <- v[topic.ind] +  leaked / length(topic.ind)
    }
    v
}


#' hubness and authority
#'
#' hub \code{h} means how important the caller is.
#'
#' authority \code{a} means how important the callee is.
#'
#' @param net network object or matrix
#' @param tol tolerance for iteration
#' @return h and a vector
#' @export
hits.rank <- function(net, tol=1e-5) {
    L <- unname(as.matrix(net))
    N <- NROW(L)
    h <- rep(1, N)
    lasth <- h
    while(TRUE) {
        a <- t(L) %*% h
        a <- a/max(a)
        h <- L %*% a
        h <- h/max(h)
        if (sum((h-lasth)^2)<tol) break
        lasth <- h
    }
    re <- cbind(h=h, a=a)
    colnames(re) <- c('h','a')
    re
}

