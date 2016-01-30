

#' Find all function calls in an R script
#'
#' @param rfile The .R input file, or a connection to read from.
#' @param include_base Whether to include calls to base functions
#'   in the output.
#' @param multiples Whether to include functions as many times as
#'   they are called.
#' @param env Environment to parse the objects into. If `NULL`,
#'   then a temporary environment is used.
#' @return Named list of character vectors.
#'   Name is the caller, contents is the callees.

parse_r_script <- function(rfile, include_base = FALSE,
                           multiples = FALSE, env = NULL) {

  if (is.null(env)) env <- new.env()

  ## Get all functions from the script
  funcs <- get_funcs_from_r_script(rfile, env = env)
  funcnames <- structure(names(funcs), names = names(funcs))

  ## Get their non-local calls
  res <- lapply(
    funcnames,
    get_global_calls,
    multiples = multiples,
    funcs = funcs,
    envir = env
  )

  ## Remove base functions, potentially
  if (!include_base) {
    res <- remove_base_functions(res)
  }

  if ("_" %in% names(res) && length(res[["_"]]) == 0) {
    res <- res[ names(res) != "_" ]
  }

  res
}


#' Extract all functions from an R script
#'
#' Reads the file into a temporary environment, and checks whether
#' the objects in this environment are functions.
#'
#' @param rfile The .R input file.
#' @param env An environment to evaluate the code in.
#'   If NULL, a new temporary environment is used.
#' @return Named list of function objects, they also include
#'   the source code, i.e. they have `srcref` attributes.

get_funcs_from_r_script <- function(rfile, env = NULL) {

  tryCatch(
    exprs <- parse(rfile, keep.source = TRUE),
    error = function (e) {
      fname <- if (is.character(rfile)) rfile else class(rfile)[1]
      warning(fname, ": ", e$message, call. = FALSE)
    }
  )

  if (is.null(env)) env <- new.env()

  funcs <- funcs_from_exprs(exprs, rfile, env = env)

  if ("_" %in% names(funcs)) {
    root <- make_func_from_exprs(funcs[names(funcs) == "_"])
    funcs <- funcs[names(funcs) != "_"]
    funcs <- c(funcs, list("_" = root))
    assign("_", root, envir = env)
  }

  funcs
}

make_func_from_exprs <- function(exprs) {
  func <- function() {}
  body(func) <- as.call(c(as.symbol("{"), exprs))
  func
}

#' Get functions from a list of expressions

funcs_from_exprs <- function(exprs, rfile, env) {
  funcs <- lapply(exprs, func_from_expr, rfile = rfile, env = env)

  res <- unlist(funcs, recursive = FALSE)

  ## Consitency
  if (length(res) == 0) {
    structure(list(), names = character())
  } else {
    res
  }
}

#' Get funcion(s) from a single expression. Usually a single function,
#' but not necessarily.

func_from_expr <- function(expr, rfile, env) {

  ## These were here before
  past <- ls(env, all.names = TRUE)

  tryCatch(
    eval(expr, envir = env),
    error = function(e) {
      fname <- if (is.character(rfile)) rfile else class(rfile)[1]
      warning(fname, ": ", e$message, call. = FALSE)
    }
  )

  keep <- Filter(
    function(x) is.function(get(x, envir = env)),
    setdiff(ls(env, all.names = TRUE), past)
  )

  if (length(keep) == 1) {
    structure(list(expr), names = keep)

  } else {
    structure(list(expr), names = "_")
  }
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

get_global_calls <- function(funcname, funcs, envir = parent.frame(),
                             include_base = TRUE, multiples = FALSE) {

  func <- get(funcname, envir = envir)

  res <- c(
    find_globals(func, multiples = multiples),
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
