
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

parse_collate <- function (str) {
  scan(text = gsub("\n", " ", str), what = "", strip.white = TRUE,
       quiet = TRUE)
}

package_collate <- function(path = ".") {
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  if ("Collate" %in% colnames(dcf)) {
    parse_collate(unname(dcf[, "Collate"]))
  } else {
    NULL
  }
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

#' Default pattern for R files
#' @return Regular expression.
#' @export

default_r_file_pattern <- function() {
  "\\.[RrSs]$"
}

#' Get all source files of a package, in the right order
#'
#' It uses the `Collate` entry in the `DESCRIPTION` file,
#' if there is one. Otherwise the order is alphabetical.

r_package_files <- function(path) {
  files <- package_collate(path)
  if (is.null(files)) {
    files <- list.files(
      file.path(path, "R"),
      pattern = default_r_file_pattern()
    )
  }

  file.path(path, "R", files)
}
