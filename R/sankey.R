
#' @importFrom sankey sankey make_sankey
#' @export

sankey_plot <- function(map, ...) {

  node_data <- node_data_frame(map)
  edge_data <- edge_data_frame(map)
  names(edge_data) <- c("from", "to", "weight")

  ## Need to remove loop edges
  edge_data <- edge_data[ edge_data[,1] != edge_data[,2], ]

  node_data$col <- ifelse(node_data$exported, "#2b8cbe", "#a6bddb")
  node_data$col[ grepl("::", node_data$ID) ] <- "#addd8e"

  sankey(make_sankey(node_data, edge_data, break_edges = TRUE), ...)
}
