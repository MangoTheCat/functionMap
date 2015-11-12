
#' Find the global functions in `do.call` patterns
#'
#' @param fun A function object.
#' @return Character vector of globals from `do.call` calls.
#'   Note that this only includes globals that are speficied
#'
#' @importFrom codetools findFuncLocals findGlobals

do_call_globals <- function(fun) {

  dc <- find_do_call_funcs(body(fun))

  globals <- findGlobals(fun, merge = FALSE)$variables
  locals <- findFuncLocals(formals(fun), body(fun))

  ## We need to report
  ## - strings that do not correspond to local variables
  ## - symbols that are global

  dc_str <- unlist(Filter(is.character, dc))
  dc_sym <- vapply(Filter(is.symbol, dc), as.character, "")

  c(setdiff(dc_str, locals), intersect(dc_sym, globals))
}

find_do_call_funcs <- function(expr) {

  L <- list()

  if (is.list(expr)) {
    for (i in seq_along(expr)) L <- c(L, find_do_call_funcs(expr[[i]]))

  } else if (is.call(expr) && identical(expr[[1]], quote(do.call))) {
    expr_formal <- match.call(do.call, expr)
    L <- c(L, expr_formal$what)
    L <- c(L, find_do_call_funcs(expr_formal$args))
    
  } else if (is.call(expr) && length(expr) > 1) {
    for (i in 2:length(expr)) L <- c(L, find_do_call_funcs(expr[[i]]))
  }

  L
}
