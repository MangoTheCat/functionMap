
as_riverplot_base <- function(x) {
  node_data <- node_data_frame(x)
  edge_data <- edge_data_frame(x)

  ext <- setdiff(unlist(x$data), names(x$data))

  node_data$x <- calculate_x_pos(x)
  node_data[match(ext, node_data$ID), ]$x <- max(node_data$x)

  int_styles <- replicate(
    length(x$data),
    list(col = "#00990099", srt = "0"),
    simplify = FALSE
  )

  ext_styles <- replicate(
    length(ext),
    list(col = "#FF000099", srt = "0"),
    simplify = FALSE
  )

  node_styles <- structure(
    c(int_styles, ext_styles),
    names = c(names(x$data), ext)
  )

  list(
    node_data = node_data,
    edge_data = edge_data,
    node_styles = node_styles
  )
}

#' Convert a function map to a Sankey plot (a \code{riverplot} object)
#'
#' A \code{riverplot} object describes a Sankey diagram, and
#' can be plotted via the \code{riverplot} package.
#'
#' @param x The function map.
#' @param ... Additional arguments to pass to the \code{makeRiver}
#'   function in the \code{riverplot} package.
#' @return A \code{riverplot} object.
#'
#' @family visualization
#' @export

as_riverplot <- function(x, ...)
  UseMethod("as_riverplot")

#' @method as_riverplot function_map
#' @export
#' @importFrom riverplot makeRiver default.style

as_riverplot.function_map <- function(x, ...) {

  river <- as_riverplot_base(x)

  makeRiver(
    river$node_data,
    river$edge_data,
    node_styles = river$node_styles,
    ...
  )
}

#' Sankey diagram of an R package call graph
#'
#' This is currently the same as the basic method.
#'
#' @inheritParams as_riverplot
#' @method as_riverplot function_map_rpackage
#' @export

as_riverplot.function_map_rpackage <- function(x, ...) {

  river <- as_riverplot_base(x)

  makeRiver(
    river$node_data,
    river$edge_data,
    node_styles = river$node_styles,
    ...
  )
}

#' Calculate the X position of the functions on a Sankey plot
#' The network must be a DAG (directed, acyclic graph) to be able to
#' perform this. (Loop edges are OK, these are removed.)
#'
#' If the function map is not a DAG, then an error is reported.
#'
#' We create the topological sort of the network first, and then
#' simply take all vertices in that order. Each vertex is assigned
#' a level one higher than the highest level of it's predecessors.
#' When no predecessors, level zero is assigned.
#'
#' @param map The function map.
#' @return A character vector, all functions in the function map,
#'   in the order of topological sorting.

calculate_x_pos <- function(map) {
  graph <- remove_loops(get_graph(map, only_me = FALSE))
  rev_graph <- twist_graph(graph)
  levels <- structure(rep(-1, length(graph)), names = names(graph))

  order <- topo_sort(graph)

  for (n in order) {
    pred_levels <- levels[ rev_graph[[n]] ]
    levels[[n]] <- if (length(pred_levels) == 0) 0 else max(pred_levels) + 1
  }

  levels
}
