
#' edgelist.from.rscript
#'
#' Difference between this function and \code{\link{parseRscript}} is this one will give more information
#' of the edge and the node.
#'
#' @param rfile R script name
#' @return The return value is a data.frame containing 5 columns.
#' \describe{
#'  \item{tails}{the caller}
#'  \item{heads}{the callee, and in the graph we will draw an arrow from tail pointing to head}
#'  \item{category}{currently can be\describe{ 
#'      \item{normal}{an ordinary function call}
#'      \item{do.call}{invoked using do.call mechanism}
#'      \item{native.call}{a native C or FORTRAN function, invoked by .C, .Fortran, .Call or .External}
#'      \item{eval.call}{invoked by \code{eval}, typically might be a variable or expression, rather than a existing function}
#'  }}
#'  \item{weights}{how many times the callee appearing in the caller}
#'  \item{rfile}{where this relation is parsed from}
#' }
#' @export
#' @examples \dontrun{
#'  rfile <- system.file("examples", "R", "func.R", package = "functionMap")
#'  edgelist.from.rscript(rfile)
#' }
edgelist.from.rscript <- function(rfile) {
	
	tmp.env <- new.env()
	res.source <- try(source(file = rfile, local = tmp.env, keep.source = TRUE), silent = TRUE)
	
	# deal with S4 methods
	if (inherits(res.source, "try-error") && grepl("^Error in setGeneric", as.character(res.source))) {
		rlines <- readLines(rfile)
		rlines[grep("setGeneric", rlines)] <- paste0("#", rlines[grep("setGeneric", rlines)] )
		rlines[grep("setMethod", rlines)] <- paste0("#", rlines[grep("setMethod", rlines)] )
		rlines.con <- textConnection(rlines)
		source(file = rlines.con, local = tmp.env, keep.source = TRUE)
	}
	
	rfile.obj <- ls(tmp.env, all.names = TRUE)
	rfile.fun <- rfile.obj[sapply(rfile.obj, FUN = function(X) class(tmp.env[[X]]) == "function")]
	
    # ensure to remove unclosed sink after exiting
    old.sink.number <- sink.number()
    on.exit(while( sink.number()>old.sink.number ) sink() )
    #
    edgelist <- data.frame('tails'=character(0), 'heads'=character(0), 'category'=character(0), 'weights'=integer(0), stringsAsFactors=FALSE)
    #
	tmp.funcall <- list()
	tmp.dir = tempdir()
	for (i in 1:length(rfile.fun)) {
		tmp.file <- tempfile(tmpdir = tmp.dir)
		tmp.con <- file(tmp.file, open = "w")
		sink(file = tmp.con, type = "output")
		print(body(get(rfile.fun[i], envir = tmp.env)))
		sink(type = "output")
		close(tmp.con)
		tmp.parsedata <- getParseData(parse(tmp.file, keep.source = TRUE))
		tmp.funcall[[i]] <- tmp.parsedata$text[tmp.parsedata$token == "SYMBOL_FUNCTION_CALL"]
    # dumpS4Generic will convert S4 methods name to funname[signatures]
        tails <- rfile.fun[i]
        if (length(tmp.funcall[[i]])) {
            heads <- table(tmp.funcall[[i]])
            el <- data.frame(tails=rfile.fun[i], heads=names(heads), category='normal', weights=as.vector(heads), stringsAsFactors=FALSE)
            edgelist <- rbind(edgelist, el)
        }
    # do.call.pattern
        re <- try(analyse.do.call.pattern(body(get(rfile.fun[i], envir=tmp.env))), silent=TRUE)
        if ( (!is(re,'try-error')) && (!is.null(re)) && length(re)>0 ) {
            re <- convertToCharacter(re)
            heads <- table(re)
            el <- data.frame(tails=rfile.fun[i], heads=names(heads), category='do.call', weights=as.vector(heads), stringsAsFactors=FALSE)
            edgelist <- rbind(edgelist, el)
            tmp.funcall[[i]] <- c(tmp.funcall[[i]], re) 
        }
    # external.call.pattern
        re <- try(analyse.external.call.pattern(get(rfile.fun[i], envir=tmp.env)), silent=TRUE)
        if ( (!is(re,'try-error')) && (!is.null(re)) && length(re)>0 ) {
            re <- convertToCharacter(re)
            heads <- table(re)
            el <- data.frame(tails=rfile.fun[i], heads=names(heads), category='native.call', weights=as.vector(heads), stringsAsFactors=FALSE)
            edgelist <- rbind(edgelist, el)
            tmp.funcall[[i]] <- c(tmp.funcall[[i]], re) 
        }
    # eval.call.pattern
        re <- try( analyse.eval.call.pattern( get(rfile.fun[i], envir=tmp.env)) , silent=TRUE )
        if ( (!is(re,'try-error')) && (!is.null(re)) && length(re)>0 ) {
            re <- convertToCharacter(re)
            heads <- table(re)
            el <- data.frame(tails=rfile.fun[i], heads=names(heads), category='eval.call', weights=as.vector(heads), stringsAsFactors=FALSE)
            edgelist <- rbind(edgelist, el)
            tmp.funcall[[i]] <- c(tmp.funcall[[i]], re) 
        }
		unlink(tmp.file, force = TRUE)
	}
	names(tmp.funcall) <- rfile.fun
	rm(tmp.env)

    edgelist$rfile <- basename(rfile)
    edgelist
}

#' edgelist.from.rpackage
#'
#' Similar to \code{\link{network.from.rpackage}}, but differences.
#' It try to give even more information on S3,S4,do.call,eval ... etc
#' 
#' Note: \code{dumpS4Generic} will pollute the .GlobalEnv, you should run the function from a clean environment
#'
#' The working horse is still \code{edgelist.from.rscript}, and the return value have the same structure of \code{\link{edgelist.from.rscript}}
#'
#' By default, '[' and hence \code{subet} will copy all the attributes, hence those attributes in the edgelist object: 
#' \code{exported.s3} and \code{exported.s4} will be copied. These two are actually parsed from NAMESPACE file.
#'
#' But for code definition of S4 methods, they can be identified by checking the \code{rfile} slot of the object: all
#' the S4 methods implemented in this package are collected to a temporary file started with "S4definition". If you use 
#' '[' or \code{subset} to get part of the edges, those information for S4 definition edges will be lost, although the 
#' information of exported S4 and S3 will still be available.
#'
#' @param base.path path to the R source, it has to be a package base path, or the result might be empty
#' @param rfilepattern some author may use extension ".[qQ][sS]" other than [Rr], this option can select from those
#' @param force.scan.s4 when the base.path is not a package, this function generally don't get any hint from NAMESPACE and if you insist, it will try hard to scan S4 information
#' @return a data.frame representing the edges of the whole pacakge
#' @export
#' @examples \dontrun{
#'    el <- edgelist.from.rpackage(system.file("examples",'packages','KernSmooth', package = "functionMap"))
#'    el2 <- edgelist.from.rpackage(system.file("examples",'packages','psych', package = "functionMap"))
#'    el.sp <- edgelist.from.rpackage(system.file("examples",'packages','sp', package = "functionMap"))
#' }

edgelist.from.rpackage <- function(base.path,  rfilepattern = "\\.[Rr]$", force.scan.s4 = TRUE) {
    S3 <- guess.s3.from.dir(base.path)
    rs <- list.files(file.path(base.path,'R'), pattern=rfilepattern, rec=TRUE, full=TRUE)
    exported.s4 <- guess.s4.from.dir(base.path)
    el.s4 <- NULL
    if (force.scan.s4 || (!is.null(exported.s4) && !is.null(exported.s4$S4.methods))) {
        s4.txt <- try(s4.source.from.rpackage(base.path, rfilepattern), silent=TRUE)
        if (!is(s4.txt, 'try-error')) {
            fn <- file.path(tempdir(),sprintf('S4definition.%s.R',basename(base.path)))
            if (length(s4.txt)>0) {
                cat(paste(s4.txt,collapse='\n'), file = fn)
                el.s4 <- edgelist.from.rscript(fn)
            }
        }
    }
    ##
    edgelist <- NULL
    # There might be possible duplicated definition of single function, we merge them all
    for(fn in rs) {
        x <- try(edgelist.from.rscript(fn), silent=TRUE)
        if (is(x,'try-error')) next
        edgelist <- rbind(edgelist, x)
    }
    if (!is.null(el.s4)) {
        edgelist <- rbind(edgelist, el.s4)
    }
    attr(edgelist, 'exported.s3') <- S3
    attr(edgelist, 'exported.s4') <- exported.s4
    ## 
    class(edgelist) <- append(class(edgelist), 'edgelist')
    edgelist
}

#' inter connected edgelists
#'
#' Subset the edgelist to a smaller one that the callees are defined in the same package.
#'
#' For each defined function(tails), add attributes of text to record other callees which are not defined in this package.
#' If a caller only calls functions outside this package, a name 'outpackage_functions' will be used to replace the actual name.
#' Weights are summarized accordingly.
#'
#' Note: after interconnected, the inpackage tagging by \code{\link{network.from.edgelist}} might be incorrect. 
#'
#' @param el edgelist
#' @return data.frame
#' @export
interconnected <- function(el) {
    r1 <- subset(el, heads %in% tails)
    r2 <- subset(el, !heads %in% tails)
    r2 <- sapply(split(r2, r2$tails), function(x) sprintf('[%s]', paste(x$heads,collapse=',')))
    defined.but.depend.on.outer <- setdiff(names(r2), r1$tails)
    for(i in defined.but.depend.on.outer) {
        ind <- which(el$tails==i)
        template <- el[ ind[1] ,  ]
        template$heads <- 'outpackage_functions'
        template$weights <- sum(el$weights[ind])
        r1 <- rbind(r1, template)
    }
    attr(r1, 'vertex.other.call') <- unlist(r2)
    r1
}

#' network.from.edgelist
#'
#' Convert edgelist to network, also denotate the attributes as S3, S4 for the vertices
#'
#' 
#'
#' @param edgelist the edgelist returned by \code{\link{edgelist.from.rpackage}}
#' @return network object with vertex attributes 
#' @examples \dontrun{
#'  el = edgelist.from.rpackage('/experiment/RNONMEM/importing/RNMImport')
#'  el
#'  nn = network.from.edgelist(el)
#'  nn
#'  plot(eForce(nn, use.network.attr=TRUE))
#'  el2 = interconnected(el)
#'  el2
#'  attr(el2,'vertex.other.call')[1:10]
#'  n2 = network.from.edgelist(el2)
#'  (n2 %v% 'details')[1:10]
#'  plot(eForce(n2, use.network.attr=TRUE))
#'  n2 %v% 'value' <- ceiling(rank(page.rank(n2)))
#'  plot(eForce(n2, use.network.attr=TRUE))
#'
#'  network.vertex.names(nn) [ rank(page.rank(nn))>400 ]
#'   
#' }
network.from.edgelist <- function(edgelist) {
    n <- network(edgelist, matrix.type='edgelist', ignore.eval=FALSE)
    S3 <- attr(edgelist, 'exported.s3')
    S4 <- attr(edgelist, 'exported.S4')
    v.names <- edgelist$tails # v.names are the callers
    if (length(ind<-grep('^S4definition\\.', edgelist$rfile))) {
        v.names.s4 <- edgelist$tails[ind]
        v.names.s4.base <- sub('\\[[^\\[\\]]+?\\]$','', v.names.s4, perl=TRUE)
        v.names.s4.signature <- substring(v.names.s4, nchar(v.names.s4.base)+1)
    } else {
        v.names.s4 <- character(0)
        v.names.s4.base <- character(0)
    }
    vall <- network.vertex.names(n)
    # create category
    inpackage <- vall %in% v.names
    exportedS3 <- vall %in% paste(S3[,1],S3[,2],sep='.')
    calledS3 <- vall %in% S3[,1]
    exportedS4 <- vall %in% v.names.s4 
    calledS4 <- vall %in% v.names.s4.base
    other <- !(inpackage | exportedS3 | calledS3 | exportedS4 | calledS4)
    mat <- cbind( inpackage , exportedS3 , calledS3 , exportedS4 , calledS4, other)
    tag <- c('inpackage','exportedS3','calledS3','exportedS4','calledS4','other')
    category <- as.vector(apply(mat, 1, function(x) paste(tag[which(x)],collapse=',')))
    if (!is.null(attr(edgelist,'vertex.other.call'))) {
        tmp <- attr(edgelist,'vertex.other.call')[ vall ]
        tmp[is.na(tmp)] <- ''
        n %v% 'details' <- as.vector(unname(tmp))
        category[ vall == 'outpackage_functions' ] <- paste(category[ vall == 'outpackage_functions' ],
            'outpackage_functions',sep=',')
    }
    n %v% 'category' <- category
    n
}
