
#' All functions of a function map of a package or script
#'
#' @param map Function map.
#' @export

functions <- function(map) {
  sort(names(map$data))
}

#' All functions that called in a package or R script
#'
#' @param map Function map.
#' @return Character vector of functions called.
#' @export

functions_called <- function(map) {
  sort(unique(unlist(map$data)))
}

#' Functions that are never used in a package
#'
#' These are the functions that are never called by an exported
#' function, neither directly, nor indirectly.
#'
#' @param map Function map.
#' @export

unused_functions <- function(map) {
  if (!is(map, "function_map_rpackage")) {
    stop("This only workds for R packages")
  }

  g <- get_graph(map)
  isolates(g, map$exports)
}

#' Dependencies of all functions
#'
#' @param map Function map.
#' @param multiples Whether to keep multiple function calls to
#'   the same function, i.e. duplicates in the list.
#' @return Named list of character vectors.
#'
#' @export

deps <- function(map, multiples = FALSE) {
  if (multiples) {
    map$data
  } else {
    lapply(map$data, unique)
  }
}

#' Reverse dependencies of all functions
#'
#' @param map Function map.
#' @param multiples Whether to keep multiple calls from the same
#'   function.
#' @return Named list of character vectors.
#'
#' @export

rev_deps <- function(map, multiples = FALSE) {
  graph <- twist_graph(get_graph(map))
  if (multiples) {
    graph
  } else {
    lapply(graph, unique)
  }
}
