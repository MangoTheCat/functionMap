
#' Dump all S4 definition from namespace
#'
#' @title dump and reformat generic functions (S4) in a namespace
#'
#' The difficulty part of statically analysing for S4 methods dispatching is it's hard to determine from code
#' which method will be called:
#'
#' For example : plot(x)
#'
#' unless we know what x is, we are not knowing what version of plot is actually invoked, hence we do not know
#' the exactly depedency relation (function Map), 
#' 
#' This function will change setMethod(methodname, signature, implementation) to the following format:
#'
#'      methodname.obj1.obj2.obj3 <- implementation
#' 
#' where obj1, obj2, obj3 are object types in the signatures
#' 
#' @param ns character name for namespace, or an environment object
#' @param style S4 style rename method as function[type1,type2,type3], S3 style is function.type1.type2.typ3
#' @return reformated text (source) file
#' @author Mango solutions
#' @examples \dontrun {
#'      ordinary.functions <- parseRfolder(system.file("examples", "R", package = "functionMap"))
#'      
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

#' wrapper for dumpS4Generic and parseRscript
#' @param ... parameters passed to dumpS4Generic
#' @return a parsed object from parseRscript
#' @export
#' @examples \dontrun{
#'      ##
#'      parseS4fromNs(style='S4')
#'      ##
#'      ordinary.functions <- parseRfolder(system.file("examples", "R", package = "functionMap"))
#'      
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

#' directed network
#'
#' allow combine plain functions and S4 function in a single network, then 
#' can be plot by plotFunctionMap
#'
#' @param plain.fun normal functions
#' @param s4list S4list dumped by dumpS4Generic
#' @return network
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
