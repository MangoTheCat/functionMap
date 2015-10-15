
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
            re <- try(analyse.do.call.pattern(body(get(rfile.fun[i], envir=tmp.env))), silent=TRUE)
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
        if ('eval.call.pattern' %in% analyse.advance.pattern) {
            re <- try( analyse.eval.call.pattern( get(rfile.fun[i], envir=tmp.env)) , silent=TRUE )
            if ( (!is(re,'try-error')) && (!is.null(re)) && length(re)>0 ) {
                tmp.funcall[[i]] <- c(tmp.funcall[[i]], re)
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
#' Helper function for \code{\link{analyse.do.call.pattern}},
#' which convert return list of \code{analyse.do.call.pattern} to character vector
#' @param L return list of \code{analyse.do.call.pattern}
#' @return character list representing input \code{L}
#' @export
convertToCharacter <- function(L) {
    # as.character(quote(a + b)) -> '+' 'a' 'b', we should use deparse
    if (is.null(L) || length(L)==0) return(character(0))
    sapply(L, function(x) if (is.language(x)) paste(deparse(x), collapse='') else x, USE.NAMES=FALSE)
}

#' analyse.do.call.pattern
#'
#' This function helps user to add additional edges to the final functional Map
#' 
#' For \code{do.call} pattern is like 
#'
#'  \code{do.call(sin, argList)} 
#' 
#' it can be analyzed staticly, without knowning run-time information, hence it can be converted to a normal function call
#' 
#' @param e an expression or list of expressions
#' @return list of expressions
#' @examples \dontrun{
#'       analyse.do.call.pattern( { 
#'           quote(do.call('myfun', list('arg1','arg2','arg3')) )
#'       })
#'       re <- analyse.do.call.pattern( { 
#'           quote(do.call(myfun, list(arg1,arg2,arg3)) )
#'       })
#'       convertToCharacter(re) }
#' @export
analyse.do.call.pattern <- function(e) {
    if (is.function(e)) {
    # currently, it's still too trick to deal with promise, so, if there is a default value which
    # is an unevaluated expression, which happened to have function call, we can not catch that.
    # We only collect the function call in do.call form in the body
        return(Recall(body(e)))
    }
    if (is.atomic(e) || is.symbol(e)) return(NULL)
    L <- NULL
    if (is.list(e)) {
        for(i in seq_along(e)) {
            L <- c(L, Recall(e[[i]]))
        }
        return(L)
    }
    if (is.call(e)) {
         if (e[[1]]=='do.call') {
             e.formal <- match.call(do.call, e)
             # There are possibilities that the "what" of "do.call" is dynamic
             # So we do not convert it to character
             # it can be character, or remain a symbol, or even a complicated expression
             L <- c(L, e.formal$what)
             # as the parseRscript has collected all normal function calls
             # we only need to scan over the args for do.call type

             L <- c(L, Recall(e.formal$args))
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

#' match.eval.pattern
#' 
#' Helper function for \code{\link{analyse.eval.call.pattern}} for recursively matching the eval pattern
#' @param e expression
#' @return \code{TRUE} or \code{FALSE} and matched eval calls
#' @examples \dontrun{
#'    match.eval.pattern( body(model.frame.default) )  }
#' @export
match.eval.pattern <- function(e) {
    if (is.symbol(e) || is.atomic(e)) return(FALSE)
    re <- FALSE
    if (is.call(e)) {
        if (e[[1]] == 'eval' || e[[1]] == 'evalq' || e[[1]] == 'eval.parent' || e[[1]] == 'local') {
            cl <- match.call(eval, e)
            re <- TRUE
            attr(re, 'eval.calls') <- list( cl )
            return(re)
        }
        for(i in 1:length(e)) {
            re1 <- Recall(e[[i]])
            if (re1) { 
                re <- structure(TRUE, eval.calls = c(attr(re, 'eval.calls'), attr(re1, 'eval.calls')))
            }
        }
    }
    re
}

#' analyse.eval.call.pattern
#'
#' Analyse and extract out any \code{eval(m)} pattern where \code{m} is a call if possible
#'
#' One pattern of eval plus literal like \code{eval(fun(arg1,arg2))} can be handled by \code{\link{parseRscript}}.
#'
#' However, a call object (in a variable) plus eval can not be handled by \code{parseRscript}.
#'
#' The real difficulty is if the call is constructed dynamically, e.g. 
#'
#' in \code{lm} function, a call to \code{stats::model.frame} is by construct a call stored in variable \code{mf}
#'
#' and then eval it.
#'
#' This dynamic behavior is almost impossible to analysis using static analysis.
#'
#' However, we can catch a particular pattern, i.e.  \code{eval(m)}
#'
#' where there is a line saying \code{m[[1]] <- some function}
#'
#' There are situations \code{ m <- match.call() } , then change some argument and call it, however
#' this time the function is the function itself, we don't count this as in the final plot, self-loop is not shown.
#'
#' @param f function or expression in a block in bracket \code{{}}
#' @param Bindings known variable bindings, typically empty
#' @param baseline.lineno recording nested structure, typically not used by user, but needed by the recursive function itself
#' @return list of the function name called by eval. There may be additional attribute having information of discovered bindings and all eval expressions
#' @examples \dontrun{
#'    analyse.eval.call.pattern(lm)
#'    analyse.eval.call.pattern(glm)
#'    analyse.eval.call.pattern(anova.mlm)
#'    # the clever things this function does is it will try to look up
#'    # symbol(variable) meaning from context, compare the following
#'    # example
#'    attr(match.eval.pattern( body(model.frame.default)), 'eval.calls')
#'    # note those fcall, predvars, extras and subset are replaced by
#'    # the value look up in the context
#'    analyse.eval.call.pattern( model.frame.default )[1:4]  }
#' @export
analyse.eval.call.pattern <- function( f , Bindings = list(), baseline.lineno=NULL) {
    if (is.function(f)) {
        f <- body(f)
        # when we deal with function, always wrap it in a block if not
        if (f[[1]] != '{') {
            f <- call('{', f)
        }
    }
    if (!is.call(f) || f[[1]] != '{') {
        if (f[[1]]=='UseMethod') {
            warning('Input is a generic method, you might want input a S3 method for specified class')
            return(character(0))
        }
        stop("Input have to be function or function body")
    }
    Evals <- list()
    Eval.Call.Pattern <- character(0)
    if (length(f)<2) {
    # possible {}
        return(Eval.Call.Pattern)
    }
    # baseline.lineno can be c(1,1,2,3)  indicating a nested structure
    for(i in 2:length(f)) {
        e <- f[[i]]
        if (is.call(e) && (e[[1]] == '=' || e[[1]] == '<-' || e[[1]] == '<<-' )) {
            Bindings[[ length(Bindings) + 1]] <- list(lhs =  e[[2]], rhs = e[[3]] , lineno = c(baseline.lineno, i) , export.to.parent = (e[[1]]=='<<-'))
        }
        if.has.eval.matched.already <- FALSE
        if (is.call(e) && (e[[1]] == 'if')) {
            if (e[[c(3,1)]] == '{') {
            # deal with if's positive branch
                foo <- Recall(e[[3]], Bindings, baseline.lineno=c(baseline.lineno, i))
                branch.p.bindings <- attr(foo,'Bindings')
                if (!is.null(branch.p.bindings)) {
                    for(k in seq_along(branch.p.bindings)) {
                        # exclude what we passed in
                        if (k<=length(Bindings)) next
                        tmp <- branch.p.bindings[[k]]
                        tmp$ifbranch <- 1
                        Bindings[[ length(Bindings) + 1]] <- tmp
                    }
                }
                if (length(foo)>0) {
                    if.has.eval.matched.already <- TRUE
                    Eval.Call.Pattern <- c(Eval.Call.Pattern, foo)
                }
            }
            if (length(e)>3 && e[[c(4,1)]] == '{') {
            # deal with if's negative branch, if there is any
                foo <- Recall(e[[4]], Bindings, baseline.lineno=c(baseline.lineno, i))
                branch.n.bindings <- attr(foo,'Bindings')
                if (!is.null(branch.n.bindings)) {
                    for(k in seq_along(branch.n.bindings)) {
                        # exclude what we passed in
                        if (k<=length(Bindings)) next
                        tmp <- branch.n.bindings[[k]]
                        # nested lineno
                        tmp$ifbranch <- -1
                        Bindings[[ length(Bindings) + 1]] <- tmp
                    }
                }
                if (length(foo)>0) {
                    if.has.eval.matched.already <- TRUE
                    Eval.Call.Pattern <- c(Eval.Call.Pattern, foo)
                }
            }
        }
        # deal with situation:
        # one branch of if-else has eval(call), but not resolved in previous section, i.e. the Binding is not found
        # in the branch locally, we may need to store it and look up it in more global perspective
        #
        # Here, we claim match.eval.pattern() which means the right hand side have to contain some "eval", or any binding in it will not be analysed
        # If there is any nested function will export variable out, we may not catch them. But if such function have eval, we will catch them
        if (!if.has.eval.matched.already && (em<-match.eval.pattern(e))) {
        # we need to exclude out the situation of nested function
            if ((e[[1]]=='=' || e[[1]]=='<-' || e[[1]] == '<<-') && 
                (e[[c(3,1)]]=='function' && e[[c(3,3,1)]]=='{')) {
                # not a simple nested function
                    foo <- Recall(e[[c(3,3)]], Bindings, baseline.lineno = c(baseline.lineno, i))
                    nested.global <- attr(foo,'Bindings')
                    if (!is.null(nested.global)) {
                        for(k in seq_along(nested.global)) {
                            # need to exclude input
                            if (k <= length(Bindings)) next
                            tmp <- nested.global[[k]]
                            if (!(tmp$export.to.parent)) next
                            Bindings[[ length(Bindings) + 1]] <- tmp
                        }
                    }
                    if (length(foo)>0) {
                        Eval.Call.Pattern <- c(Eval.Call.Pattern, foo)
                    }
            } else {
                for(j in 1:length(attr(em, 'eval.calls'))) {
                    Evals[[ length(Evals) + 1 ]] <- list(expr = attr(em, 'eval.calls')[[j]]$expr , lineno = c(baseline.lineno, i))
                }
            }
        }
    }
    # we dont deal with anonymous function here
    if (length(Evals)<1) {
    # even if no eval, but binding information is useful
        attr(Eval.Call.Pattern, 'Bindings') <- invisible(Bindings)
        return(Eval.Call.Pattern)
    }

    binding.lineno <- lapply(Bindings, function(x) x$lineno)

    simplify <- function(e) {
        if (is.atomic(e)) return(e)
        if (is.symbol(e)) return(as.character(e))
        if (is.call(e) && (e[[1]] == 'quote' || e[[1]] == 'call')) {
            return(Recall(e[[2]]))
        }
        if (is.call(e)) {
            t1 <- try(eval(e), silent=TRUE)
            if (!is(t1,'try-error')) {
                if (is.character(t1)) {
                    return(t1)
                } else if (is.symbol(t1) || (is.call(t1)&&(t1[[1]]=='::'||t1[[1]]==':::'))) {
                    return(paste(deparse(t1),collapse=''))
                }
            }
        }
        if (is.call(e) && e[[1]]=='parse') {
            cl <- match.call(parse, e)
            if (!is.null(cl$text)) {
                # parse(text = )
                return(Recall(cl$text))
            }
        }
        paste(deparse(e), collapse='')
    }

    lexical.smaller <- function(n, m) {
        for(i in 1: min(length(n),length(m))) {
            if (n[i]>m[i]) return(FALSE)
            if (n[i]<m[i]) return(TRUE)
        }
        length(n) < length(m)
    }

    for(i in 1:length(Evals)) {
        expr <- Evals[[i]]$expr
        if (!is.symbol(expr)) {
        # This is a normal call
        # like eval( fun(x1,x2,x3) ) => fun(x1,x2,x3)
        # eval(1) => 1
        # Which can be catched by normal parseRscript
        # we can't do anymore look up on it, but just simplify it and add to result
            Eval.Call.Pattern <- c(Eval.Call.Pattern, simplify(expr))
            next
        } 
        # expr is a symbol, we need to 
        # looking backup
        lineno <- Evals[[i]]$lineno
        ind <- rev(which(sapply(binding.lineno, function(x) lexical.smaller(x, lineno))))
        True.call.fun <- NULL
        for(j in ind) {
            lhs <- Bindings[[j]]$lhs
            if (is.symbol(lhs) && lhs == expr) {
                True.call.fun <- Bindings[[j]]$rhs
            # rhs is the whole call which will be evaluated
                if (is.call(True.call.fun) && 
                    True.call.fun[[1]]=='call') {
                    # we try to match mf <- call('ff', args) pattern here
                        True.call.fun <- True.call.fun[[2]]
                }
                break
            }
            # Typically some.call[[1]] <- fun.name
            if (    is.call(lhs) && 
                   (lhs[[1]] == '[' || lhs[[1]]=='[[') && 
                    lhs[[2]]==expr && 
                    lhs[[3]]==1 ) {
            # this should be the function name
                True.call.fun <- Bindings[[j]]$rhs
                break
            }
        }
        # True.call.fun might be a expression or a symbol
        # However, if it is a call, which evaluated to a character, 
        # it is usually determined at runtime
        # Which is not possible be determined by static analysing
        # We try to simplify it, however, if not possible,
        # We simply deparse it to a string, and return the string
        if (!is.null(True.call.fun)) {
        # It's possible the True.call.fun cannot be found in the Binding, we ignore it
            Eval.Call.Pattern <- c(Eval.Call.Pattern, simplify(True.call.fun))
        }
    }
    attr(Eval.Call.Pattern,'Bindings') <- invisible(Bindings)
    attr(Eval.Call.Pattern,'Evals'   ) <- invisible(Evals)
    Eval.Call.Pattern
}
