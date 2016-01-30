
#' Names of all functions in the `base` package
#'
#' @return Character vector.
#' @keywords internal

get_base_funcs <- function() {
  ls(asNamespace("base"), all.names = TRUE)
}

#' Remove base function names form a character vector, or a list
#' of character vectors
#'
#' @param funcs Character vector, or list of character vectors.
#' @return Filtered character vector, or list of character vectors.
#' @keywords internal

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
