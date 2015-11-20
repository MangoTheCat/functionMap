
in_list <- function(elem, list) {
  for (e in list) if (identical(elem, e)) return(TRUE)
  FALSE
}

#' Drop NULL elements from a list
#' @param x input list

drop_null <- function(x) {
  Filter(function(xx) !is.null(xx), x)
}

#' @importFrom utils untar

extract_if_needed <- function(path) {

  if (!file.exists(path)) stop("File does not exist: ", path)

  info <- file.info(path)
  if (info$isdir) return(path)

  if (!grepl("\\.tar\\.gz$", path)) {
    warning("Package file without .tar.gz extension, continueing anyway")
  }

  tmp <- tempfile()
  untar(path, exdir = tmp)

  tmp
}

check_pkg_dir <- function(path = ".") {

  if (!file.exists(file.path(path, "DESCRIPTION"))) {
    stop("No DESCRIPTION file, is this an R package?")
  }

  if (!file.exists(file.path(path, "NAMESPACE"))) {
    stop("No NAMESPACE file, is this an R package?")
  }

  rdir <- file.path(path, "R")

  if (!file.exists(rdir)) {
    stop("No R folder, is this an R package?")
  }

  if (!file.info(rdir)$isdir) {
    stop(rdir, " is not a folder, is this an R package?")
  }

  if (length(list.files(rdir)) == 0) {
    stop(rdir, " is empty, no R source files")
  }

  if (length(list.files(rdir, pattern = "\\.[r|R]$")) == 0) {
    stop(rdir, " has no .R or .r files")
  }
}

package_name <- function(path = ".") {
  unname(read.dcf(file.path(path, "DESCRIPTION"))[, "Package"])
}

find_in_named_list <- function(list, elem) {
  for (n in names(list)) {
    if (elem %in% list[[n]]) return(n)
  }
  NA_character_
}

str_trim <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}
