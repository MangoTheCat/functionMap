
#' Find calls to external functions
#'
#' It supports `.C`, `.Call`, `.Fortran` and `.External` calls.
#'
#' @param fun Function object.
#' @param multiples Whether to include functions as many
#'   times as they are called.
#' @return Character vector of external function names,
#'   prefixed according to the call type.

external_calls <- function(fun, multiples = FALSE) {
  res <- find_external_calls(body(fun))
  if (multiples) res else unique(res)
}

find_external_calls <- function(expr, multiples) {

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
    sub(pattern = "^\\.", replacement = "", as.character(expr[[1]])),
    sep = "_",
    as.character(expr[[2]])
  )
}
