
find_function <- function(func, env = parent.env()) {

  res <- if (identical(env, emptyenv())) {
    NA_character_
  } else {
    if (exists(func, env, inherits = FALSE)) {
      environmentName(env)
    } else {
      find_function(func, parent.env(env))
    }
  }

  if (grepl("^imports", res)) {
    pkg <- sub("imports:", "", res, fixed = TRUE)
    ns <- getNamespaceImports(pkg)
    res <- find_in_named_list(ns, func)
  } else if (grepl("^package:", res)) {
    res <- sub("package:", "", res, fixed = TRUE)
  }

  res
}

parse_depends <- function(pkg) {
  deps <- packageDescription(pkg)$Depends
  if (is.null(deps)) return(character())
  deps <- strsplit(deps, ",")[[1]]
  deps <- sub("\\(.*\\)", "", deps)
  setdiff(str_trim(deps), "R")
}

attach_q <- function(x) {
  suppressMessages(suppressPackageStartupMessages(attachNamespace(x)))
}

load_dependencies <- function(pkg) {
  try(lapply(parse_depends(pkg), attach_q), silent = TRUE)
  try(attach_q(pkg), silent = TRUE)
}

unload_dependencies <- function(pkg) {
  try(unloadNamespace(pkg), silent = TRUE)
  try(lapply(parse_depends(pkg), unloadNamespace), silent = TRUE)
}

with_package <- function(path, expr) {

  package <- package_name(path)

  tmp <- tempfile()
  lib_dir <- dirname(tmp)

  on.exit(try(unlink(tmp, recursive = TRUE), silent = TRUE), add = TRUE)
  install.packages(path, lib = lib_dir, repos = NULL, type = "source",
                   quiet = TRUE)

  lp <- .libPaths()
  on.exit(try(.libPaths(lp), silent = TRUE), add = TRUE)
  .libPaths(c(lib_dir, lp))

  on.exit(try(unloadNamespace(package), silent = TRUE), add = TRUE)
  loadNamespace(package)

  on.exit(try(unload_dependencies(package), silent = TRUE), add = TRUE)
  load_dependencies(package)

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

actively_find_funcs <- function(map, funcs) {

  wh <- is.na(funcs[,2])
  functions <- funcs[wh, 1]

  name <- package_name(map$rpath)

  if (isNamespaceLoaded(name)) {
    warning(name, " is already loaded, trying to unload.")
    unloadNamespace(name)
  }

  found <- with_package(
    map$rpath,
    vapply(functions, find_function, "", env = asNamespace(name))
  )


  funcs[wh, 2] <- found

  funcs
}
