
#' Table of imported functions
#'
#' First column is the name of the function, second is the
#' name of the package it is imported from.
#'
#' @param path Root folder of the R package.
#' @return Table of imports
#' @keywords internal

get_imports <- function(path) {
  ns <- parseNamespaceFile(basename(path), file.path(path, ".."))
  imp <- ns$imports

  tab <- vapply(imp, character(2), FUN = function(x) {
    if (length(x) == 1) {
      c("*", x[[1]])
    } else {
      c(x[[2]], x[[1]])
    }
  })

  t(tab)
}

#' Get exported functions from a package, from `NAMESPACE`
#'
#' The result includes S3 methods, in the usual
#' `generic.method` notation.
#'
#' @param path Path a package root.
#' @param funcs Character vector of function names, parsed
#'   from the source files. This is needed in case there is an
#'   export pattern in the `NAMESPACE` file
#' @return Character vector of exported functions.
#' @keywords internal

get_exports <- function(path, funcs) {
  ns <- parseNamespaceFile(basename(path), file.path(path, ".."))
  exp <- ns$exports
  s3 <- paste(ns$S3methods[,1], sep = ".", ns$S3methods[,2])
  exp_pattern <- lapply(ns$exportPatterns, function(p) {
    grep(p, funcs, value = TRUE)
  })
  unique(c(ns$exports, s3, unlist(exp_pattern)))
}

#' Check if a list of function nameds were imported from a package
#'
#' @param path The folder containing the (uncompressed) R package.
#' @param functions The functions to find, a character vector.
#' @return A character vector, the names of the environments
#'   for the \code{functions}.
#' @keywords internal

find_imports <- function(path, functions) {
  imports <- get_imports(path)
  res <- imports[,2][match(functions, imports[,1])]
}

#' Try to find where some called functions live
#'
#' @details
#' \enumerate{
#'   \item They might be in the mapped package.
#'   \item They might be explicit calls to a package using `::`.
#'   \item They might be functions imported by name or in a package
#'     that was imported by a whole.
#'   \item They might be in a base package that is attached by default.
#' }
#'
#' @param map Function map object.
#' @return Data frame with columns:
#'   \item{func}{Function name.}
#'   \item{place}{Name of the package it is coming from.
#'     Empty string means it is in the mapped package. NA means that
#'     we don't know where it is coming from.}
#' @keywords internal

where <- function(map) {

  if (!inherits(map, "function_map_rpackage")) {
    stop("This is not a map of an R package")
  }

  name <- package_name(map$rpath)

  ## Function in this package
  mine <- names(map$data)

  funcs <- data.frame(
    stringsAsFactors = FALSE,
    func = sort(unique(unlist(map$data)))
  )

  ## Myself
  funcs$myself <- ifelse(funcs$func %in% mine, "", NA_character_)

  ## Explicit namespace or external calls
  funcs$known <- ifelse(
    grepl("::", funcs$func),
    sub("::.*$", "", funcs$func),
    NA_character_
  )

  ## Calls to imported functions
  funcs$imports <- find_imports(map$rpath, funcs$func)

  ## Calls to base packages (loaded by default)
  base_pkgs <- c(
    "stats", "graphics", "grDevices", "utils",
    "datasets", "methods", "base"
  )
  for (p in base_pkgs) {
    ct <- ls(paste0("package:", p))
    funcs[[p]] <- ifelse(funcs$func %in% ct, p, NA_character_)
  }

  funcs <- collapse_nas(funcs)
  if (any(is.na(funcs[,2]))) {
    funcs <- actively_find_funcs(map, funcs)
  }
  funcs[,2][is.na(funcs[,2])] <- "???"

  funcs
}

#' @importFrom stats na.omit

collapse_nas <- function(x) {
  data.frame(
    stringsAsFactors = FALSE,
    func = x$func,
    place = apply(x, 1, function(xx) na.omit(xx)[2])
  )
}

#' Add namespace information to a map of an R package
#'
#' Try to find out where the called functions live,
#' and add this information to the map.
#'
#' @param map Function map.
#' @return The annotated map.
#' @keywords internal

add_namespaces <- function(map) {
  map$exports <- get_exports(map$rpath, functions(map))

  wh <- where(map)

  for (i in seq_along(map$data)) {
    map$data[[i]]$to <- prefix_names(map$data[[i]]$to, wh)
  }

  map
}

prefix_names <- function(names, table) {
  wh <- table[match(names, table[,1]), 2]
  ifelse(
    grepl("::", names) | wh == "" | substr(wh, 1, 1) == '.',
    names,
    paste0(wh, "::", names)
  )
}
