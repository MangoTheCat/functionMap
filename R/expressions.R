
#' Find all (non-local) function calls in a function
#'
#' @param func Function object.
#'
#' @keywords internal

find_globals <- function(func) {
  calls <- find_globals_multiple(func)
  match_to_parse_data(calls, func)
}

#' Find all (non-local) function calls in a function, keep multiple calls
#'
#' @inheritParams find_globals
#' @return Character vector of function calls.
#'
#' @importFrom codetools findGlobals
#' @keywords internal

find_globals_multiple <- function(func) {

  L <- character()
  globals <- findGlobals(func, merge = FALSE)$functions

  get_calls <- function(expr) {
    if (is.call(expr)) {
      if (is.character(expr[[1]]) || is.symbol(expr[[1]])) {
        n <- as.character(expr[[1]])
        if (n %in% globals) L <<- c(L, n)
      }
    }
  }

  walk_lang(func, get_calls)
  L
}

#' Get global function calls from a function
#'
#' Note the results are approximate only. R's dynamic nature
#' does not allow us to always find the global calls reliably.
#'
#' Calls can be: \itemize{
#'   \item Direct function calls.
#'   \item Function calls via \code{do.call}.
#'   \item Calls to external functions via \code{.C}, \code{.Call}, etc.
#'   \item We assume that an S3 generic calls all its methods
#'     in the supplied environment.
#' }
#'
#' Internally we use \code{\link[codetools]{findGlobals}} for finding
#' the global calls and variables.
#'
#' Note that by default calls to base function are also included in the
#' result, even of they are primitive functions (e.g. \code{<-},
#' \code{==}, etc.).
#'
#' @param func The function to examine.
#' @param funcname Name of the function.
#' @param envir The environment containing the function. This environment
#'   is also used to look for S3 methods.
#' @return A data frame of function calls and call types.
#' @keywords internal

get_global_calls <- function(func, funcname, envir = parent.frame()) {

  df <- function(
    to, ty = "call",
    l = NA_integer_, c1 = NA_integer_, c2 = NA_integer_) {
    data_frame(to = to, type = ty, line = l, col1 = c1, col2 = c2 )
  }

  res <- rbind(
    find_globals(func),
    df(double_colon_calls(func)),
    df(func_arg_globals(func), "call"),
    df(external_calls(func), "external"),
    df(s3_calls(funcname, envir = envir), "s3")
  )

  res
}
