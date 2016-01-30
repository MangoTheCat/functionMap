
#' Find the global functions that are used as arguments
#'
#' In \code{do.call}, \code{apply}, \code{lapply}, \code{sapply},
#' \code{vapply}, \code{tapply}, \code{mapply}, \code{eapply},
#' \code{rapply} functions.
#'
#' @param fun A function object.
#' @param multiples Whether to include functions as many
#'   times as they are called.
#' @return Character vector of globals from these calls.
#'
#' @importFrom codetools findFuncLocals findGlobals
#' @keywords internal

func_arg_globals <- function(fun, multiples = FALSE) {

  funcs <- list(
    c(fun = "do.call", arg = "what"),
    c(fun = "apply", arg = "FUN"),
    c(fun = "lapply", arg = "FUN"),
    c(fun = "sapply", arg = "FUN"),
    c(fun = "vapply", arg = "FUN"),
    c(fun = "tapply", arg = "FUN"),
    c(fun = "mapply", arg = "FUN"),
    c(fun = "eapply", arg = "FUN"),
    c(fun = "rapply", arg = "f")
  )

  dc <- unlist(
    lapply(funcs, find_func_arg_globals, expr = body(fun)),
    recursive = FALSE
  )

  globals <- findGlobals(fun, merge = FALSE)$variables
  locals <- findFuncLocals(formals(fun), body(fun))

  ## We need to report
  ## - strings that do not correspond to local variables
  ## - symbols that are global

  dc_str <- unlist(Filter(is.character, dc))
  dc_sym <- vapply(Filter(is.symbol, dc), as.character, "")

  if (multiples) {
    c(dc_str[ ! dc_str %in% locals ], dc_sym[ dc_sym %in% globals ])
  } else {
    unique(c(setdiff(dc_str, locals), intersect(dc_sym, globals)))
  }
}

find_func_arg_globals <- function(needle, expr) {

  need_fun <- as.symbol(needle[["fun"]])
  need_arg <- needle[["arg"]]
  need_obj <- get(needle[["fun"]], envir = asNamespace("base"))

  L <- list()

  get_calls <- function(expr) {
    if (is.call(expr) && identical(expr[[1]], need_fun)) {
      expr_formal <- match.call(need_obj, expr)
      fun <- expr_formal[[need_arg]]
      if (is.character(fun) || is.symbol(fun)) {
        L <<- c(L, expr_formal[[need_arg]])
      }
    }
  }

  walk_lang(expr, get_calls)
  L
}
