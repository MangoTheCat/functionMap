
#' Find the S3 methods of a function, in an environment
#'
#' Typically used to find the S3 methods defined in a package,
#' for an S3 generic, also defined in the package.
#'
#' @param funcname Name of the S3 generic.
#' @param envir Environment containing functions, we search in this.
#' 
#' @importFrom pryr is_s3_generic is_s3_method
#' @keywords internal

s3_calls <- function(funcname, envir) {
  if (!is_s3_generic(funcname, envir)) return(character())

  s3_candidates <- Filter(
    function(x) maybe_s3_method(funcname, x),
    ls(envir)
  )

  Filter(
    function(x) is_s3_method(x, env = envir),
    s3_candidates
  )
}

#' Predicate to pre-filter possible S3 methods
#'
#' An S3 method of a generic must start with the name of the generic,
#' then have a dot, and some extra characters. It must also call
#' `UseMethod`, but that is not checked here, but elsewhere.
#' 
#' @param generic Name of the S3 generic.
#' @param method Name of the alleged S3 method.
#' @return Logical scalar.
#' @keywords internal

maybe_s3_method <- function(generic, method) {
  nc <- nchar(generic)
  nchar(method) >= nc + 2 &&
    substr(method, 1, nc) == generic &&
    substr(method, nc + 1, nc + 1) == "."
}
