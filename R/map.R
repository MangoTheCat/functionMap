
#' Create a function_map object from a parsed result
#'
#' @param data A named list. Names are function names,
#'   list entries are the functions they call. Make sure you include
#'   multiplicity.
#' @param package Name of the R package, or \code{NULL} if not an R
#'   package.
#' @param rfile Name of the R script, or \code{NULL} if not an R script.
#' @param rpath Path to the R folder, or \code{NULL} is not an R folder.
#' @param rfilepattern Pattern for R files in the folder, or \code{NULL}
#'   if not an R folder.
#' @param include_base Whether calls to base functions are included.
#' @param class What class to set on the result, in addition to
#'   \code{function_map}.
#' @return A function_map object.
#' @keywords internal

create_function_map <- function(data, package = NULL,
                                rfile = NULL, rpath = NULL,
                                rfilepattern = NULL, include_base = NULL,
                                class = NULL) {
  res <- structure(
    list(
      rfile = rfile,
      rpath = rpath,
      package = package,
      rfilepattern = rfilepattern,
      include_base = include_base,
      data = data
    ),
    class = c(class, "function_map")
  )

  if (class == "function_map_rpackage") res <- add_namespaces(res)

  res$node_df <- node_data_frame(res$data, exports = res$exports)
  res$edge_df <- edge_data_frame(res$data)
  res$data <- NULL

  res
}

#' Map the function calls for an R script
#'
#' @inheritParams parse_r_script
#' @return A function_map object.
#'
#' @export

map_r_script <- function(rfile, include_base = FALSE) {
  create_function_map(
    rfile = rfile,
    include_base = include_base,
    class = "function_map_rfile",
    data = parse_r_script(
      rfile,
      include_base)
  )
}


#' Map the function calls for a folder of R scripts
#'
#' @inheritParams parse_r_folder
#' @return A function_map object.
#'
#' @export

map_r_folder <- function(rpath, rfilepattern = default_r_file_pattern(),
                         include_base = FALSE) {
  create_function_map(
    rpath = rpath,
    rfilepattern = rfilepattern,
    class = "function_map_rfolder",
    data = parse_r_folder(
      rpath,
      rfilepattern,
      include_base
    )
  )
}

#' Map an R package
#'
#' @param path Name of a source R package tar archive file,
#'   or path to the folder of an R package.
#' @inheritParams parse_r_folder
#'
#' @export

map_r_package <- function(path, include_base = FALSE) {

  ## Special case, it messes up parseNamespaceFile and others
  if (path == "." || path == "./") path <-getwd()

  ## Extract it to a temporary directory if it is a file
  path <- extract_if_needed(path)

  ## Check if basic structure is OK, has R code,
  ## has DESCRIPTION, NAMESPACE, etc.
  check_pkg_dir(path)

  name <- package_name(path)

  ## Everyting is parsed into this
  env <- new.env()

  create_function_map(
    package = name,
    rpath = path,
    class = "function_map_rpackage",
    data = parse_r_folder(
      rpath = r_package_files(path),
      rfilepattern = default_r_file_pattern(),
      include_base = include_base,
      env = env
    )
  )
}
