
#' Table of imported functions
#'
#' First column is the name of the function, second is the
#' name of the package it is imported from.
#'
#' @param path Root folder of the R package.
#' @return Table of imports

get_imports <- function(path) {
  name <- package_name(path)
  imp <- parseNamespaceFile(name, file.path(path, ".."))$imports

  tab <- vapply(imp, character(2), FUN = function(x) {
    if (length(x) == 1) {
      c("*", x[[1]])
    } else {
      c(x[[2]], x[[1]])
    }
  })

  t(tab)
}

get_exports <- function(path) {
  name <- package_name(path)
  parseNamespaceFile(name, file.path(path, ".."))$exports
}

#' Find functions imported from a package
#'
#' @param path The folder containing the (uncompressed) R package.
#' @param functions The functions to find, a character vector.
#' @return A character vector, the names of the environments
#'   for the \code{functions}.

find_imports <- function(path, functions) {
  imports <- get_imports(path)
  res <- imports[,2][match(functions, imports[,1])]
}

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

add_namespaces <- function(map) {
  map$exports <- get_exports(map$rpath)

  wh <- where(map)

  for (i in seq_along(map$data)) {
    map$data[[i]] <- prefix_names(map$data[[i]], wh)
  }

  map
}

prefix_names <- function(names, table) {
  wh <- table[match(names, table[,1]), 2]
  ifelse(wh == "" | substr(wh, 1, 1) == '.', names, paste0(wh, "::", names))
}
