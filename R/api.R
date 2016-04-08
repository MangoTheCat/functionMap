
#' Extract data about the functions in a function map
#'
#' Returns a data frame, where each row is a function in the function
#' map. The data frame includes functions that are in (other) R
#' packages, and are called from the mapped script, folder or package.
#'
#' @param map The function map object.
#' @return A data frame with one row for each function in the map.
#' Current columns:
#'   \item{ID}{The name of the function.}
#'   \item{own}{Whether it is a function in the mapped script, folder or
#'     package.}
#'   \item{exported}{Whether it is an exported function of the package.
#'     For scripts and folders it is always \code{FALSE}.}
#'
#' @export

node_df <- function(map) {
  map$node_df
}

#' Extract data about the edges (function calls) in a function map
#'
#' @param map The function map.
#' @return A data frame with columns:
#'   \item{from}{Calling function.}
#'   \item{to}{Called function.}
#'   \item{weight}{Number of calls from the caller to the callee.}
#'
#' @export

edge_df <- function(map) {
  map$edge_df
}

#' All functions of a function map of a package or script
#'
#' @param map Function map.
#' @export

functions <- function(map) {
  ndf <- node_df(map)
  sort(ndf$ID[ ndf$own ])
}

#' All functions that called in a package or R script
#'
#' @param map Function map.
#' @return Character vector of functions called.
#' @export

functions_called <- function(map) {
  edf <- edge_df(map)
  sort(unique(edf$to))
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
    stop("This only works for R packages")
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
    deps_multiples(map)

  } else {
    deps_no_multiples(map)
  }
}

deps_multiples <- function(map) {
  func <- functions(map)
  edges <- edge_df(map)
  structure(
    lapply(func, function(f) {
      my <- edges$from == f
      rep(edges$to[my], edges$weight[my])
    }),
    names = func
  )
}

deps_no_multiples <- function(map) {
  func <- functions(map)
  edges <- edge_df(map)
  structure(
    lapply(func, function(f) {
      edges$to[ edges$from == f]
    }),
    names = func
  )
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
