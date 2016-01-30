
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
#' @keywords internal

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

#' Create a function from a list of expressions
#'
#' We do this, so that `codetools::findGlobals` can be
#' called on the function.
#'
#' @param exprs The list of expressions to put into the
#'   function body.
#' @return A function.
#' @keywords internal

make_func_from_exprs <- function(exprs) {
  func <- function() {}
  body(func) <- as.call(c(as.symbol("{"), exprs))
  func
}

#' Get functions from a list of expressions
#'
#' @param exprs List of expressions.
#' @param rfile Name of the R file being parsed, to be able to
#'   use it in a warning message.
#' @param env Environment to store the parsed objects in.
#' @return A named list of expressions.
#' @keywords internal

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
#'
#' @details
#' We need to handle expressions that are not simple function
#' definitions. This is the current algorithm: \itemize{
#'   \item We evaluate the expression in \code{env}.
#'   \item Then we check if there is anything new in \code{env}.
#'   \item If there is a single new function, then we assume that
#'     the expression is the definition of this function, and
#'     use the function's name as expression name.
#'   \item Otherwise (zero or more than one new functions),
#'     we assume that the expression is not a function defition
#'     and we assign it to the function body (\code{_}).
#' }
#' 
#' @param expr Expression to evaluate.
#' @inheritParams funcs_from_exprs
#' @return A named list of length one. (It is easy to concatenate
#'   these lists in the caller, hence the format. A list with a
#'   name and an expression is less convenient.)
#' @keywords internal

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
