
#' @importFrom pryr is_s3_generic is_s3_method

s3_calls <- function(funcname, envir, multiples = FALSE) {
  if (!is_s3_generic(funcname, envir)) return(character())

  s3_candidates <- Filter(
    function(x) maybe_s3_method(funcname, x),
    ls(envir)
  )

  res <- Filter(
    function(x) is_s3_method(x, env = envir),
    s3_candidates
  )

  if (!multiples) unique(res) else res
}

maybe_s3_method <- function(generic, method) {
  nc <- nchar(generic)
  nchar(method) >= nc + 2 &&
    substr(method, 1, nc) == generic &&
    substr(method, nc + 1, nc + 1) == "."
}
