

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
    base_funcs <- ls(asNamespace("base"), all.names = TRUE)
    res <- lapply(
      res,
      function(x) { x[ ! x %in% base_funcs ] }
    )
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

  find_globals_multiple_x <- function(expr) {

    L <- character()

    if (is.list(expr)) {
      for (e in expr) L <- c(L, find_globals_multiple_x(e))

    } else if (is.call(expr) && as.character(expr[[1]]) %in% globals) {
      L <- c(L, as.character(expr[[1]]))
      L <- c(L, find_globals_multiple_x(expr[-1]))

    } else if (is.call(expr) && length(expr) > 1) {
      for (i in 2:length(expr)) L <- c(L, find_globals_multiple_x(expr[[i]]))
    }

    L
  }

  globals <- findGlobals(func, merge = FALSE)$functions
  find_globals_multiple_x(body(func))
}

get_global_calls <- function(func, multiples = FALSE) {
  res <- c(
    find_globals(func, multiples = multiples),
    do_call_globals(func, multiples = multiples),
    external_calls(func, multiples = multiples)
  )

  if (!multiples) res <- unique(res)

  res
}
