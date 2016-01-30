

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
