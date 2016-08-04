
#' Extract a package tarball
#'
#' The package is extracted in a temporary directory.
#'
#' If `path` is a directory, then we do nothing.
#' @param path Path to a package tarball, or a package directory.
#' @return Path to the temporary directory (or the supplied `path`
#'   if it was already a directory.
#' @keywords internal
#'
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

#' Check if a path is an R package root directory
#'
#' @details
#' It must have: \itemize{
#'   \item `DESCRIPTION`
#'   \item `NAMESPACE`
#'   \item A non-empty `R` folder.
#' }
#'
#' @param path Path to the alleged package root.
#' @keywords internal

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

#' Get the name of the package from `DESCRIPTION`
#'
#' @param path Path to the package root.
#' @return Package name
#' @keywords internal

package_name <- function(path = ".") {
  unname(read.dcf(file.path(path, "DESCRIPTION"))[, "Package"])
}
