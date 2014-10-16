
##' Parse the R script and return the function structure
##'
##' @title Parse the R script and return the function structure.
##' @param rfile Path of the R script file.
##' @return A list of all the functions included in this script. Each component contains the names of the functions were called by this function. 
##' @author Mango Solutions
##' @examples \dontrun{
##' rfile <- system.file("examples", "R", "func.R", package = "functionMap")
##' parseRscript(rfile)
##' }
##' 

parseRscript <- function(rfile) {
	
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
		unlink(tmp.file, force = TRUE)
	}
	names(tmp.funcall) <- rfile.fun
	rm(tmp.env)

	return(tmp.funcall)
}

#' allow user to decide if we should add additional edges in the final functionMap
#' 
#' For do.call(sin, argList), it's actually a static call, which can be converted to normal call
#' However, we analysis and created additional edges
#'
#' assuming e is one expression
create.edges.from.do.call <- function(e){
    if (is.atomic(e) || is.symbol(e)) return(NULL)
    L <- NULL
    if (is.list(e)) {
        for(i in seq_along(e)) {
            L <- c(L, Recall(e[[i]]))
        }
        return(L)
    }
    if (is.function(e)) {
    # currently, it's still too trick to deal with promise, so, if there is a default value which
    # is an unevaluated expression, which happened to have function call, we can not catch that.
    # We only collect the function call in do.call form in the body
        return(Recall(body(e)))
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

#' convert return list of create.edges.from.do.call to character
#' because  as.character(quote(a + b)) -> '+' 'a' 'b', we should use deparse
convertToCharacter <- function(L) {
    sapply(L, function(x) if (is.language(x)) paste(deparse(x), collapse='') else x, USE.NAMES=FALSE)
}

# eval plus normal call can be handled by parseRscript
# we need to consider call plus eval
# However, the real difficulty is if the call is constructed dynamically, e.g. 
# in lm function, a call to stats::model.frame is by construct a call called "mf"
# and then eval it.
# This dynamic behavior is almost impossible to analysis using static analysis

# however, we can catch a particular pattern, i.e. 
# eval(m)
# where there is a line saying m[[1]] <- some function

# recursive match
eval.matcher <- function(e) {
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

analyse.eval.pattern <- function( f , Bindings = list()) {
    if (is.function(f)) f <- body(f)
    if (!is.call(f) || f[[1]] != '{') {
        if (f[[1]]=='UseMethod') {
            warning('Input is a generic method, you might want input a S3 method for specified class')
            return(character(0))
        }
        stop("Input have to be function or function body")
    }
    Evals <- list()
    Eval.Call.Pattern <- character(0)
    for(i in 2:length(f)) {
        e <- f[[i]]
        if (is.call(e) && (e[[1]] == '=' || e[[1]] == '<-' || e[[1]] == '<<-' )) {
            Bindings[[ length(Bindings) + 1]] <- list(lhs =  e[[2]], rhs = e[[3]] , lineno = i , is.global= (e[[1]]=='<<-'))
        }
        if.has.eval.matched.already <- FALSE
        if (is.call(e) && (e[[1]] == 'if')) {
            if (e[[c(3,1)]] == '{') {
            # deal with if's positive branch
                foo <- Recall(e[[3]], Bindings)
                branch.p.bindings <- attr(foo,'Bindings')
                if (!is.null(branch.p.bindings)) {
                    for(k in seq_along(branch.p.bindings)) {
                        tmp <- branch.p.bindings[[k]]
                        # nested lineno
                        tmp$lineno <- c(i, tmp$lineno)
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
                foo <- Recall(e[[4]], Bindings)
                branch.n.bindings <- attr(foo,'Bindings')
                if (!is.null(branch.n.bindings)) {
                    for(k in seq_along(branch.n.bindings)) {
                        tmp <- branch.n.bindings[[k]]
                        # nested lineno
                        tmp$lineno <- c(i, tmp$lineno)
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
        if (!if.has.eval.matched.already && (em<-eval.matcher(e))) {
        # we need to exclude out the situation of nested function
            if ((e[[1]]=='=' || e[[1]]=='<-' || e[[1]] == '<<-') && 
                (e[[c(3,1)]]=='function' && e[[c(3,3,1)]]=='{')) {
                # not a simple nested function
                    foo <- Recall(e[[c(3,3)]], Bindings)
                    nested.global <- attr(foo,'Bindings')
                    if (!is.null(nested.global)) {
                        for(k in seq_along(nested.global)) {
                            tmp <- nested.global[[k]]
                            if (!tmp$is.global) next
                            # nested lineno
                            tmp$lineno <- c(i, tmp$lineno)
                            Bindings[[ length(Bindings) + 1]] <- tmp
                        }
                    }
                    if (length(foo)>0) {
                        Eval.Call.Pattern <- c(Eval.Call.Pattern, foo)
                    }
            } else {
                for(j in 1:length(attr(em, 'eval.calls'))) {
                    Evals[[ length(Evals) + 1 ]] <- list(expr = attr(em, 'eval.calls')[[j]]$expr , lineno = i)
                }
            }
        }
    }
    # we dont deal with anonymous function here
    if (length(Evals)<1) return(Eval.Call.Pattern)
    binding.lineno <- sapply(Bindings, function(x) x$lineno[1])

    simplify <- function(e) {
        if (is.atomic(e)) return(e)
        if (is.symbol(e)) return(as.character(e))
        if (is.call(e) && e[[1]] == 'quote') {
            return(Recall(e[[2]]))
        }
        if (is.call(e)) {
            t1 <- try(eval(e), silent=TRUE)
            if (!is(t1,'try-error')) {
                if (is.character(t1)) {
                    return(r1)
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
        ind <- rev(which(binding.lineno < lineno))
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
