
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
