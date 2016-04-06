
skip_without_package <- function(pkg, msg = NULL) {

  if (is.null(msg)) msg <- paste("Need package", pkg)

  if (pkg %in% loadedNamespaces()) return()

  on.exit(try(unloadNamespace(pkg), silent = TRUE), add = TRUE)
  if (requireNamespace(pkg, quietly = TRUE)) return()

  skip(msg)
}
