
#' Create a function_map object from a parsed result
#'
#' @param data A named list. Names are function names,
#'   list entries are the functions they call. Make sure you include
#'   multiplicity.
#' @inheritParams map_r_script
#' @inheritParams map_r_folder
#' @return A function_map object.

create_function_map <- function(data, rfile = NULL, rpath = NULL,
                                rfilepattern = NULL, include_base = NULL,
                                class = NULL) {
  structure(
    list(
      rfile = rfile,
      rpath = rpath,
      rfilepattern = rfilepattern,
      include_base = include_base,
      data = data
    ),
    class = c(class, "function_map")
  )
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
      include_base,
      multiples = TRUE)
  )
}


#' Map the function calls for a folder of R scripts
#'
#' @inheritParams parse_r_folder
#' @return A function_map object.
#'
#' @export

map_r_folder <- function(rpath, rfilepattern = "\\.[R|r]$",
                         include_base = FALSE) {
  create_function_map(
    rpath = rpath,
    rfilepattern = rfilepattern,
    class = "function_map_rfolder",
    data = parse_r_folder(
      rpath,
      rfilepattern,
      include_base,
      multiples = TRUE
    )
  )
}
