

#' Find all function calls in an R script
#'
#' @param rfile The .R input file, or a connection to read from.
#' @param include_base Whether to include calls to base functions
#'   in the output.
#' @param env Environment to parse the objects into. If `NULL`,
#'   then a temporary environment is used.
#' @return Named list of data frames, initially with columns:
#'   \code{func} and \code{type}. Type can be currently \code{call} and
#'   \code{s3}, the latter denoting an S3 generic instead of a regular
#'   function call.
#'
#' @keywords internal

parse_r_script <- function(rfile, include_base = FALSE, env = NULL) {

  if (is.null(env)) env <- new.env()

  ## Get all functions from the script
  funcs <- get_funcs_from_r_script(rfile, env = env)

  ## Get their non-local calls
  res <- mapply(
    FUN = get_global_calls,
    funcs,
    names(funcs),
    MoreArgs=list(envir = env),
    SIMPLIFY = FALSE
  )

  ## Remove base functions, potentially
  if (!include_base) {
    res <- lapply(res, remove_base_functions)
    res <- lapply(res, reset_row_names)
  }

  ## Add file name to both nodes and edges
  for (i in seq_along(res)) {
    res[[i]] <- data_frame(res[[i]], file = rfile)
    attr(res[[i]], "pos") <-
      modifyList(attr(funcs[[i]], "pos"), list(file = rfile))
  }

  ## Remove _ body, if nothing is called from there
  res <- res[ names(res) != "_" | vapply(res, NROW, 1L) != 0 ]

  ## Collapse body into a single "caller"
  if (any(names(res) == "_")) {
    body <- do.call(rbind, res[names(res) == "_"])
    res <- res[ names(res) != "_" ]
    res$`_` <- body
    rownames(res$`_`) <- NULL
  }

  res
}
