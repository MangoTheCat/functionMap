
#' Find all (non-local) function calls in a function
#' 
#' @param func Function object.
#' @param multiples Whether to keep multiplicity.
#' @return Character vector of function calls.
#'
#' @importFrom codetools findGlobals
#' @keywords internal

find_globals <- function(func, multiples = FALSE) {
  if (multiples) {
    find_globals_multiple(func)

  } else {
    findGlobals(func, merge = FALSE)$functions
  }
}

#' Find all (non-local) function calls in a function, keep multiple calls
#'
#' @param func Function object.
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
#' @param funcname Name of the function to study.
#' @param funcs All expressions parsed from the file or package.
#' @param envir The environment containing the function. This environment
#'   is also used to look for S3 methods.
#' @param include_base Whether to include calls to base functions
#'   in the output.
#' @param multiples Whether to keep multiplicity in the result. I.e.
#'   if this argument is \code{TRUE} and \code{func} calls \code{foobar}
#'   twice, then \code{foobar} is included in the result twice.
#' @return A character vector with the names of all functions called.
#' @keywords internal

get_global_calls <- function(funcname, funcs, envir = parent.frame(),
                             include_base = TRUE, multiples = FALSE) {

  func <- get(funcname, envir = envir)

  res <- c(
    find_globals(func, multiples = multiples),
    double_colon_calls(func, multiples = multiples),
    func_arg_globals(func, multiples = multiples),
    external_calls(func, multiples = multiples),
    s3_calls(funcname, multiples = multiples, envir = envir)
  )

  if (!multiples) res <- unique(res)

  if (!include_base) {
    res <- remove_base_functions(res)
  }

  res
}
