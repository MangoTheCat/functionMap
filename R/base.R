
#' Names of all functions in the `base` package
#'
#' @return Character vector.
#' @keywords internal

get_base_funcs <- function() {
  ls(asNamespace("base"), all.names = TRUE)
}

#' Remove base function names form a data frame of functions
#'
#' @param funcs Data frame, first column is the function names.
#'   Other columns are extra metadata, which we do not use.
#' @return Filtered character vector, or list of character vectors.
#' @keywords internal

remove_base_functions <- function(funcs) {
  base_funcs <- get_base_funcs()
  funcs[ ! funcs[,1] %in% base_funcs, ]
}
