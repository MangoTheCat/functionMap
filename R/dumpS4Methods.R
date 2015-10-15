
#' dumpS4Generic
#'
#' Dump all S4 definition from namespace
#'
#' The difficulty part of static analysis for S4 methods dispatching is it's hard to determine from code
#' which method will be called:
#'
#' For example : \code{plot(x)}
#'
#' unless we know what \code{x} is, we are not knowing what version of \code{plot} is actually invoked, hence we do not know
#' the exactly depedency relation (function Map), 
#' 
#' This function will change \code{setMethod(methodname, signature, implementation)} to the following format:
#'
#'      \code{methodname.obj1.obj2.obj3 <- implementation}
#' 
#' where \code{obj1, obj2, obj3} are object types in the signatures
#'
#' @title dump and reformat generic functions (S4) in a namespace
#' @param ns character name for namespace, or an environment object
#' @param style S4 style rename method as \code{function[type1,type2,type3]}, S3 style is \code{function.type1.type2.typ3}
#' @return reformated text (source) file
#' @author Mango solutions
#' @examples \dontrun{
#'
#'      ordinary.functions <- parseRfolder(system.file("examples", "R", package = "functionMap"))
#'      # need to eval those functions to make the definition into .GlobalEnv
#'      for(i in list.files(system.file("examples", "R", package = "functionMap"),full.names=TRUE, pattern='*.R')) source(i)
#'      S4.funs <- dumpS4Generic()
#'      cat(paste(S4.funs,collapse='\n'), file=(f1<-tempfile()))
#'      all.funs <- c(ordinary.functions, parseRscript(f1))
#'      
#'      nets <- createNetwork(all.funs)
#'      plot(eForce(nets))
#'
#' }
#' @export
dumpS4Generic <- function(ns, style=c('S4','S3')) {
    if (missing(ns)) ns <- .GlobalEnv
    if (is.environment(ns)) {
        env <- ns
    } else {
        if (is.character(ns)) {
            if (ns=='.GlobalEnv') {
                env <- .GlobalEnv
            } else {
                env <- asNamespace(ns)
            }
        } else {
            stop('ns need to be an environment or namespace')
        }
    }

    style <- match.arg(style)

    generics <- getGenerics(env)
    
    txts <- character()

    for(fg in generics) {
        mm <- findMethods(fg, where=env )
        signatures <- findMethodSignatures(methods = mm)
        tmpfile <- tempfile()
        for(j in NROW(signatures)) {
            dumpMethod(fg, signatures[j,], file=tmpfile, where=env)
            s <- readLines(tmpfile)
            s <- s[-c(1,length(s))] # remove head and tail, only keep body
            if (style=='S3') {
                s1 <- sprintf('`%s` <- ', paste(c(fg,signatures[j,]),collapse='.'))
            } else {
                s1 <- sprintf('`%s[%s]` <- ', fg, paste(signatures[j,],collapse=','))
            }
            txts <- c(txts, s1, s, '\n')
        }
    }

    txts
}

#' parseS4fromNs
#'
#' wrapper for \code{\link{dumpS4Generic}} and \code{\link{parseRscript}}
#' @param ... parameters passed to \code{\link{dumpS4Generic}}
#' @return a parsed object from \code{parseRscript}
#' @export
#' @examples \dontrun{
#'      ## Matrix is a very complex S4 package
#'      parseS4fromNs('Matrix')
#'      ##
#'      ordinary.functions <- parseRfolder(system.file("examples", "R", package = "functionMap"))
#'      ## if there are generics definition in .GlobalEnv, you can omit it
#'      for(i in list.files(system.file("examples", "R", package = "functionMap"),full.names=TRUE, pattern='*.R')) source(i)
#'      parseS4fromNs()
#'      ##
#'      all.funs <- c(ordinary.functions, parseS4fromNs())
#'      
#'      nets <- createNetwork(all.funs)
#'      plot(eForce(nets))
#'      
#' }
parseS4fromNs <- function(...) {
    txts <- dumpS4Generic(...)
    if (length(txts)==0) return(NULL)
    cat(paste(txts,collapse='\n'), file = (f<-tempfile()))
    parseRscript(f)
}

#' createDirectedNetwork 
#'
#' Combine plain functions and S4 function in a single network, which 
#' can be plot by \code{\link{plotFunctionMap}}
#'
#' @param plain.fun normal functions
#' @param s4list S4list dumped by \code{\link{dumpS4Generic}}
#' @examples \dontrun{
#'
#'      ordinary.functions <- parseRfolder(system.file("examples", "R", package = "functionMap"))
#'      # because the setClass and setMethod are actually evaluated in .GlobalEnv in above
#'      for(i in list.files(system.file("examples", "R", package = "functionMap"),full.names=TRUE, pattern='*.R')) source(i)
#'      S4.funs <- parseS4fromNs()
#'      
#'      nets <- createDirectedNetwork(ordinary.functions, S4.funs)
#'      plotFunctionMap(nets, TRUE)
#'
#'
#' } 
#' @return network object with directed arrows, if A used in its body B, then there should be an arrow pointing from B to A
#' @export
createDirectedNetwork <- function(plain.fun, s4list=list(), if.directed=TRUE) {
    plain.fun.nms <- unique(names(plain.fun))
    s4.entire.nms <- unique(names(s4list))
    s4.main.nms   <- sub('\\[.*$','',s4.entire.nms)
    V <- c(plain.fun.nms, s4.entire.nms)
    
    m <- matrix(0, length(V), length(V), dimnames=list(V,V))
    for(i in 1:length(V)) {
        v <- V[i]
        used <- plain.fun[[v]]
        if (is.null(used)) {
            used <- s4list[[v]]
        }
        if (is.null(used)) next
        m[i, ] <- colSums(outer(used, V, '=='))
    }
    network(m, directed=if.directed)
}


#' extract.S4.defn
#'
#' Extract S4 class and methods definition
#'
#' @param srclist source list
#' @param path alternatively, we can specify a path and use all R sources in it
#' @param single if we should paste the multiple characters into a single one
#' @param export.other.defn export other function definitions
#' @return text include only S4 related definitions 
#' @examples \dontrun{
#'
#'      cat(extract.S4.defn(path=system.file("examples", "R", package = "functionMap")))
#' }
#' @export
extract.S4.defn <- function(srclist, path, single=TRUE, export.other.defn=TRUE) {
    if (missing(srclist)) {
        if (missing(path)) stop('Must specify at least scrlist or path as input.')
        srclist <- list.files(path, full.names=TRUE, pattern='\\.[Rr]$')
    }
    L <- do.call('c', sapply(srclist, parse))

    if (export.other.defn) {
        # extract assignments of other ordinary functions to global env
        # if those definitions are not available, the extracted S4 statement may fail 
        # if you try to evaluate them on .GlobalEnv
        L.ordinary <- L[ sapply(L, class) == '<-' ]
        # we need to populate function definitions to environment, or setGeneric may fail
        for(i in seq_along(L.ordinary)) {
            try(eval(L.ordinary[[i]], envir=.GlobalEnv), silent=TRUE)
        }
        #
    }
    L <- L[ sapply(L, class)=='call' ]
    # class of call, can be setClass, setMethod, setGeneric, or other ordinary call
    set.function.types <- c(
        'setOldClass',
        'setClass',
        'setClassUnion',
        'setAs',
        'setGeneric',
        'setGenericImplicit',
        'setGroupGeneric',
        'setIs',
        'setLoadAction',
        'setLoadActions',
        'setMethod',
        'setPackageName',
        'setRefClass',
        'setReplaceMethod',
        'setValidity')

    cls <- as.character(sapply(L, el, 1))
    L <- L[ cls %in% set.function.types ] 
    cls <- cls[ cls %in% set.function.types ] 
    ind <- split(seq_along(cls), cls)
    
    output.txt <- c()
    # insert setClass first
    if (!is.null(ind$setClass))
        output.txt <- c(output.txt, paste(sapply(L[ind$setClass], deparse),collapse='\n'))
    # then the generic
    if (!is.null(ind$setGeneric))
        output.txt <- c(output.txt, paste(sapply(L[ind$setGeneric], deparse),collapse='\n'))
    # finally others
    output.txt <- c(output.txt, paste(sapply(L[ unlist(ind[  ! (names(ind) %in% c('setClass','setGeneric'))]) ], deparse),collapse='\n'))
    
    if (single) output.txt <- paste(output.txt, collapse='\n')

    output.txt
}
