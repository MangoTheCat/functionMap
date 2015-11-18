
#' Create a function_map object from a parsed result
#'
#' @param parsed A named list. Names are function names,
#'   list entries are the functions they call. Make sure you include
#'   multiplicity.
#' @return A function_map object.

create_function_map <- function(parsed) {
  class(parsed) <- "function_map"
  parsed
}

#' Map the function calls for an R script
#'
#' @inheritParams parse_r_script
#' @return A function_map object.
#'
#' @export

map_r_script <- function(rfile, include_base = FALSE) {
  create_function_map(
    parse_r_script(
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
    parse_r_folder(
      rpath,
      rfilepattern,
      include_base,
      multiples = TRUE
    )
  )
}
