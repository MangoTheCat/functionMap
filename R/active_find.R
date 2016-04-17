
#' Evaluate an expression with a package loaded
#'
#' The package will be installed into a temporary directory.
#' Then all required dependencies will be loaded, and the package
#' will be loaded as well.
#'
#' After the expression is evaluated, all dependencies and the package
#' itself will be unloaded, and the temporary library directory
#' will be deleted. The library path will be also restored.
#'
#' @param path Path to the package directory and tarball to load.
#' @param expr Expression to evaluate.
#' @return Value of the evaluated expression.
#'
#' @importFrom remotes install_local
#' @keywords internal

with_package <- function(path, expr) {

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  message("Installing package ", package_name(path), " to temporary library")
  install_local(path, lib = lib, quiet = TRUE)
  loadNamespace(package_name(path), lib.loc = lib)

  expr
}

#' Try harder to find functions that are called
#'
#' We try to load imported and depended packages
#' and search their namespaces as well.
#'
#' For this we need to install the package to a temporary
#' directory, and load it. If this is not possible, a warning
#' is given.
#'
#' We asume that the trivial search in imported packages
#' and base packages did not work, so we don't try it again.
#'
#' @param map Function map.
#' @param funcs Character vector, functions to find.
#' @keywords internal

actively_find_funcs <- function(map, funcs) {

  wh <- is.na(funcs[,2])
  functions <- funcs[wh, 1]

  name <- package_name(map$rpath)

  found <- with_package(
    map$rpath,
    mget(
      functions,
      envir = asNamespace(name),
      inherits = TRUE,
      ifnotfound = NA_character_
    )
  )

  pkgs <- vapply(found, FUN.VALUE = "", function(x) {
    if (is.function(x)) {
      environmentName(environment(x))
    } else {
       NA_character_
    }
  })

  funcs[wh, 2] <- pkgs

  funcs
}
