
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

#' Alternative to data.frame
#'
#' * Sets stringsAsFactors to FALSE by default
#' * If any columns have zero length, the result will have
#'   zero rows.
#' * If a column is a scalar, then it will be recycled.
#' * Non-matching number of rows gives an error, except for
#'   lengths zero and one.
#'
#' @param ... Named data frame columns, or data frames.
#' @param stringsAsFactors Just leave it on FALSE. :)
#' @return Data frame.
#'
#' @keywords internal

data_frame <- function(..., stringsAsFactors = FALSE) {
  cols <- list(...)
  stopifnot(length(cols) > 0)

  len <- vapply(cols, NROW, 1L)
  maxlen <- max(len)
  stopifnot(all(len %in% c(0, 1, maxlen)))

  ## recycle, only scalars. If one empty, all empty
  res_len <- if (0 %in% len) 0 else maxlen
  cols2 <- lapply(cols, function(x) myrep(x, res_len))
  names(cols2) <- names(cols)

  res <- do.call(
    data.frame,
    c(cols2, list(stringsAsFactors = stringsAsFactors))
  )
  reset_row_names(res)
}

#' Recycle a vector or a data frame (rows)
#'
#' @param x Vector or data frame to replicate. Must be length 0, 1, or
#'   len.
#' @param len Expected length.
#'
#' @keywords internal

myrep <- function(x, len) {

  stopifnot(len == 0 || NROW(x) == len || NROW(x) == 1)

  if (NROW(x) == len) {
    x

  } else if (is.data.frame(x)) {
    x[ rep(1, len), ]

  } else {
    rep(x, length.out = len)
  }
}

reset_row_names <- function(df) {
  rownames(df) <- NULL
  df
}
