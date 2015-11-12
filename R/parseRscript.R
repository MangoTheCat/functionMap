

#' Find all function calls in an R script
#'
#' @param rfile The .R input file, or a connection to read from.
#' @param include_base Whether to include calls to base functions
#'   in the output.
#' @return Named list of character vectors.
#'   Name is the caller, contents is the callees.

parse_r_script <- function(rfile, include_base = FALSE) {

  ## Get all functions from the script
  funcs <- get_funcs_from_r_script(rfile)

  ## Get their non-local calls
  res <- lapply(funcs, get_global_calls)

  ## Remove base functions, potentially
  if (!include_base) {
    res <- lapply(res, setdiff, ls(asNamespace("base"), all.names = TRUE))
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

get_global_calls <- function(func) {
  c(
    findGlobals(func, merge = FALSE)$functions,
    do_call_globals(func),
    external_calls(func)
  )
}
