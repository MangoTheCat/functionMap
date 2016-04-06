
#' Sankey plot of a function map
#'
#' Sankey plots usually show the flow of some quantity, but they can also
#' be used to visualize functions calls between functions.
#'
#' By default exported functions in a package are colored dark blue, and
#' packages called from another package are colored green.
#'
#' Recursive calls are omitted from the plot.
#'
#' @param map Function map.
#' @param ... Additional arguments are passed to \code{sankey::sankey}.
#' @return Nothing.
#'
#' @importFrom sankey sankey make_sankey
#' @export

sankey_plot <- function(map, ...) {

  node_data <- node_df(map)
  edge_data <- edge_df(map)
  names(edge_data) <- c("from", "to", "weight")

  ## Need to remove loop edges
  edge_data <- edge_data[ edge_data[,1] != edge_data[,2], ]

  node_data$col <- ifelse(node_data$exported, "#2b8cbe", "#a6bddb")
  node_data$col[ grepl("::", node_data$ID) ] <- "#addd8e"

  sankey(make_sankey(node_data, edge_data, break_edges = TRUE), ...)
}
