
map_r_package <- function(path, include_base = FALSE) {

  ## Some base R functions only work on normalized paths
  path <- normalizePath(path)

  ## Extract it to a temporary directory if it is a file
  path <- extract_if_needed(path)

  ## Check if basic structure is OK, has R code,
  ## has DESCRIPTION, NAMESPACE, etc.
  check_pkg_dir(path)

  name <- package_name(path)

  
}
