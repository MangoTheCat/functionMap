
#' Find the global functions in `do.call` patterns
#'
#' @param fun A function object.
#' @return Character vector of globals from `do.call` calls.
#'   Note that this only includes globals that are speficied
#'   in the `do.call` call as a character. Globals that are passed
#'   to `do.call` as function objects, are found by the standard
#'   `findGlobals` already.
#'
#' @importFrom codetools findFuncLocals findGlobals

do_call_globals <- function(fun) {

  dc_funcs <- find_do_call_funcs(body(fun))

  if (length(dc_funcs) > 0) {
    locals <- findFuncLocals(formals(fun), body(fun))
    globals <- findGlobals(fun)

  }

  dc_funcs
}

find_do_call_funcs <- function(expr) {

  L <- character()

  if (is.list(expr)) {
    for (i in seq_along(expr)) L <- c(L, find_do_call_funcs(expr[[i]]))

  } else if (is.call(expr) && identical(expr[[1]], quote(do.call))) {
    expr_formal <- match.call(do.call, expr)
    if (is.atomic(expr_formal$what)) L <- c(L, expr_formal$what)
    L <- c(L, find_do_call_funcs(expr_formal$args))
    
  } else if (is.call(expr) && length(expr) > 1) {
    for (i in 2:length(expr)) L <- c(L, find_do_call_funcs(expr[[i]]))
  }

  L
}
