
#' Find calls to external functions
#'
#' It supports `.C`, `.Call`, `.Fortran` and `.External` calls.
#'
#' @param fun Function object.
#' @return Character vector of external function names,
#'   prefixed according to the call type.
#' @keywords internal

external_calls <- function(fun) {
  find_external_calls(body(fun))
}

find_external_calls <- function(expr) {

  external_calls <- list(
    quote(.C),
    quote(.Call),
    quote(.Fortran),
    quote(.External)
  )

  L <- character()

  get_external <- function(expr) {
    if (is.call(expr) && in_list(expr[[1]], external_calls)) {
      L <<- c(L, extract_external_func(expr))
    }
  }

  walk_lang(expr, get_external)
  L

}

extract_external_func <- function(expr) {
  paste(
    expr[[1]],
    sep = "::",
    as.character(expr[[2]])
  )
}
