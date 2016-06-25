
#' Extract all functions from an R script
#'
#' Reads the file into a temporary environment, and checks whether
#' the objects in this environment are functions.
#'
#' @param rfile The .R input file.
#' @param env An environment to evaluate the code in.
#'   If NULL, a new temporary environment is used.
#' @return Named list of function objects, they also include
#'   the source code, in a parsed data base form, as attribute
#'   \sQuote{src}.
#' @keywords internal

get_funcs_from_r_script <- function(rfile, env = NULL) {

  exprs <- list()
  tryCatch(
    {
      exprs <- parse(rfile, keep.source = TRUE)
      exprs <- parse(rfile, keep.source = TRUE)
    },
    error = function (e) {
      fname <- if (is.character(rfile)) rfile else class(rfile)[1]
      warning(fname, ": ", e$message, call. = FALSE)
    }
  )

  for (i in seq_along(exprs)) {
    if (is.null(exprs[[i]])) next
    attr(exprs[[i]], "src") <- extract_src_attr(exprs, i)
  }

  if (is.null(env)) env <- new.env()

  funcs <- funcs_from_exprs(exprs, rfile, env = env)

  funcs
}

extract_src_attr <- function(exprs, num) {
  pd <- getParseData(exprs, includeText = TRUE)
  tops <- which(pd$parent == 0)
  first <- tops[num]
  last <- if (num < length(tops)) tops[num + 1] - 1 else nrow(pd)
  pd[first:last, , drop = FALSE ]
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
#' If the expression has a \code{function} token and at least
#' one assignment, then we assume that it is a function definition
#' and evaluate it to get the function name.
#' This is the current algorithm: \itemize{
#'   \item We evaluate the expression in \code{env}.
#'   \item Then we check if there is anything new in \code{env}.
#'   \item If there is a single new function, then we assume that
#'     the expression is the definition of this function, and
#'     use the function's name as expression name.
#'   \item Otherwise (zero or more than one new functions),
#'     we assume that the expression is not a function defition
#'     and we assign it to the function body (\code{_}).
#'
#' Othewise we assume it is not a function definition.
#' }
#'
#' @param expr Expression to evaluate.
#' @inheritParams funcs_from_exprs
#' @return A named list of length one. (It is easy to concatenate
#'   these lists in the caller, hence the format. A list with a
#'   name and an expression is less convenient.)
#' @keywords internal

func_from_expr <- function(expr, rfile, env) {

  parseData <- attr(expr, "src")

  func <- if ("FUNCTION" %in% parseData$token &&
      any(c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN") %in% parseData$token)) {
    eval_to_get_func_name(expr, rfile, env)

  } else {
    list(
      "_" = structure(
        wrap_in_function(expr),
        src = attr(expr, "src") %||% empty_parse_data()
      )
    )
  }

  attr(func[[1]], "pos") <- get_function_position(func[[1]])

  func
}

empty_parse_data <- function() {
  getParseData(parse(text = "", keep.source = TRUE))
}

get_function_position <- function(func) {

  pd <- attr(func, "src")
  line <- which(pd$token == "FUNCTION")[1]
  list(
    line = pd$line1[line] %||% NA_integer_,
    col1 = pd$col1[line] %||% NA_integer_,
    col2 = pd$col2[line] %||% NA_integer_
  )
}

eval_to_get_func_name <- function(expr, rfile, env) {

  ## These were here before
  past <- ls(env, all.names = TRUE)

  ## We don't warn for errors currently. We could, but then for
  ## packages we would need to load the package to get the
  ## dependencies.
  tryCatch(
    eval(expr, envir = env),
    error = function(e) {
      fname <- if (is.character(rfile)) rfile else class(rfile)[1]
      ## warning(fname, ": ", e$message, call. = FALSE)
    }
  )

  keep <- Filter(
    function(x) is.function(get(x, envir = env)),
    setdiff(ls(env, all.names = TRUE), past)
  )

  if (length(keep) == 1) {
    structure(
      list(structure(get(keep, env), src = attr(expr, "src"))),
      names = keep
    )

  } else {
    list("_" = structure(
           wrap_in_function(expr),
           src = attr(expr, "src")) %||% empty_parse_data()
         )
  }
}

wrap_in_function <- function(expr) {
  f <- function() { }
  body(f) <- expr
  f
}
