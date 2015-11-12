
##' Parse the R script and return the function structure
##'
##' @title Parse the R script and return the function structure.
##' @param rfile Path of the R script file.
##' @param analyse.advance.pattern any extra pattern we should also analyse and include
##' @return A list of all the functions included in this script. Each component contains the names of the functions were called by this function. 
##' @author Mango Solutions
##' @examples \dontrun{
##' rfile <- system.file("examples", "R", "func.R", package = "functionMap")
##' parseRscript(rfile)
##' }
##' 

parseRscript <- function(rfile, analyse.advance.pattern =getOption('analyse.advance.pattern')) {
	
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
        if ('do.call.pattern' %in% analyse.advance.pattern) {
            re <- try(do_call_globals(get(rfile.fun[i], envir=tmp.env)), silent=TRUE)
            if ( (!is(re,'try-error')) && (!is.null(re)) && length(re)>0 ) {
                tmp.funcall[[i]] <- c(tmp.funcall[[i]], convertToCharacter(re)) 
            }
        }
        if ('external.call.pattern' %in% analyse.advance.pattern) {
            re <- try(analyse.external.call.pattern(get(rfile.fun[i], envir=tmp.env)), silent=TRUE)
            if ( (!is(re,'try-error')) && (!is.null(re)) && length(re)>0 ) {
                tmp.funcall[[i]] <- c(tmp.funcall[[i]], convertToCharacter(re))
            }
        }
		unlink(tmp.file, force = TRUE)
	}
	names(tmp.funcall) <- rfile.fun
	rm(tmp.env)

	return(tmp.funcall)
}


#' convertToCharacter
#'
#' Convert a list names and characters to a character vector.
#'
#' @param L input list.
#' @return character list representing input \code{L}
#' @export
convertToCharacter <- function(L) {
    # as.character(quote(a + b)) -> '+' 'a' 'b', we should use deparse
    if (is.null(L) || length(L)==0) return(character(0))
    sapply(L, function(x) if (is.language(x)) paste(deparse(x), collapse='') else x, USE.NAMES=FALSE)
}
#' analyse.external.call.pattern
#'
#' match \code{.C}, \code{.Fortran} and \code{.Call}
#'
#' A global option \code{add.prefix.for.external.call} will add \code{C_}, \code{FORTRAN_} and \code{External_} prefix or not
#'
#' @param e expression
#' @return list of experssions
#' @examples \dontrun{
#'     analyse.external.call.pattern( quote( .C('classRF') ) )
#'     analyse.external.call.pattern( quote( .C(classRF) ) )
#' }
analyse.external.call.pattern <- function(e){
    if (is.function(e)) return(Recall(body(e)))
    if (is.atomic(e) || is.symbol(e)) return(NULL)
    L <- NULL
    if (is.list(e)) {
        for(i in seq_along(e)) {
            L <- c(L, Recall(e[[i]]))
        }
        return(L)
    }
    if (is.call(e)) {
         if (e[[1]]=='.C' || e[[1]]=='.Fortran' || e[[1]]=='.Call' || e[[1]] == '.External' ) {
         # we can't use match.call for primitive call
         # assume position 1 is always the .NAME of the routine being called
            if (isTRUE(getOption('add.prefix.for.external.call'))) {
                prefix <- switch(as.character(e[[1]]),
                    '.C' = 'C',
                    '.Fortran' = 'FORTRAN',
                    'EXTERNAL')
                L <- c(L, paste(prefix, convertToCharacter(e[[2]]), sep='_'))
            } else {
                L <- c(L, e[[2]])
            }
         } else {
             if (length(e)>1) {
                 for(i in 2:length(e)) {
                     L <- c(L, Recall(e[[i]]))
                 }
             }
         }
    }
    L
}
