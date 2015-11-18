
walk_lang <- function(x, f, ...) {

  f(x, ...)

  if (is.function(x)) {
    walk_lang(formals(x), f, ...)
    walk_lang(body(x), f, ...)

  } else if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x)) {
    lapply(x, walk_lang, f = f, ...)
  }

  invisible()
}
