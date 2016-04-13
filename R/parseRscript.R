

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

parse_r_script <- function(rfile, include_base = FALSE,
                           env = NULL) {

  if (is.null(env)) env <- new.env()

  ## Get all functions from the script
  funcs <- get_funcs_from_r_script(rfile, env = env)
  funcnames <- structure(names(funcs), names = names(funcs))

  ## Get their non-local calls
  res <- lapply(
    funcnames,
    get_global_calls,
    funcnames = funcnames,
    funcs = funcs,
    envir = env
  )

  ## Remove base functions, potentially
  if (!include_base) {
    res <- lapply(res, remove_base_functions)
    res <- lapply(res, reset_row_names)
  }

  ## Remove _ body, if nothing is called from there
  res[ names(res) != "_" | vapply(res, NROW, 1L) != 0 ]
}
