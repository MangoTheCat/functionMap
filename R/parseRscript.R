

#' Find all function calls in an R script
#'
#' @param rfile The .R input file, or a connection to read from.
#' @param include_base Whether to include calls to base functions
#'   in the output.
#' @param multiples Whether to include functions as many times as
#'   they are called.
#' @return Named list of character vectors.
#'   Name is the caller, contents is the callees.

parse_r_script <- function(rfile, include_base = FALSE,
                           multiples = FALSE) {

  ## Get all functions from the script
  funcs <- get_funcs_from_r_script(rfile)

  ## Get their non-local calls
  res <- lapply(funcs, get_global_calls, multiples = multiples)

  ## Remove base functions, potentially
  if (!include_base) {
    res <- remove_base_functions(res)
  }

  res
}


#' Extract all functions from an R script
#'
#' Reads the file into a temporary environment, and checks whether
#' the objects in this environment are functions.
#'
#' @param rfile The .R input file.
#' @return Named list of function objects, they also include
#'   the source code, i.e. they have `srcref` attributes.

get_funcs_from_r_script <- function(rfile) {

  ## Load everything into a temporary environment
  tmp_env <- new.env()
  tryCatch(
    source(rfile, local = tmp_env),
    error = function(e) {
      fname <- if (is.character(rfile)) rfile else class(rfile)[1]
      warning(fname, ": ", e$message, call. = FALSE)
    }
  )

  ## Keep the functions
  keep <- Filter(
    function(x) is.function(get(x, envir = tmp_env)),
    ls(tmp_env, all.names = TRUE)
  )

  mget(keep, envir = tmp_env)
}

#' @importFrom codetools findGlobals

find_globals <- function(func, multiples = FALSE) {
  if (multiples) {
    find_globals_multiple(func)

  } else {
    findGlobals(func, merge = FALSE)$functions
  }
}

#' @importFrom codetools findGlobals

find_globals_multiple <- function(func) {

  L <- character()
  globals <- findGlobals(func, merge = FALSE)$functions

  get_calls <- function(expr) {
    if (is.call(expr) && (n <- as.character(expr[[1]])) %in% globals) {
      L <<- c(L, n)
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
#' }
#'
#' Internally we use \code{\link[codetools]{findGlobals}} for finding
#' the global calls and variables.
#'
#' Note that by default calls to base function are also included in the
#' result, even of they are primitive functions (e.g. \code{<-},
#' \code{==}, etc.).
#'
#' @param func Function object.
#' @param include_base Whether to include calls to base functions
#'   in the output.
#' @param multiples Whether to keep multiplicity in the result. I.e.
#'   if this argument is \code{TRUE} and \code{func} calls \code{foobar}
#'   twice, then \code{foobar} is included in the result twice.
#' @return A character vector with the names of all functions called.
#'
#' @export

get_global_calls <- function(func, include_base = TRUE, multiples = FALSE) {

  res <- c(
    find_globals(func, multiples = multiples),
    do_call_globals(func, multiples = multiples),
    external_calls(func, multiples = multiples)
  )

  if (!multiples) res <- unique(res)

  if (!include_base) {
    res <- remove_base_functions(res)
  }

  res
}

get_base_funcs <- function() {
  ls(asNamespace("base"), all.names = TRUE)
}

remove_base_functions <- function(funcs) {

  base_funcs <- get_base_funcs()

  if (is.list(funcs)) {
    lapply(
      funcs,
      function(x) { x[ ! x %in% base_funcs ] }
    )
  } else {
    funcs[ ! funcs %in% base_funcs ]
  }
}
