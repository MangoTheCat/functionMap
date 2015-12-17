
#' Extract vertex (= function) metadata from the call graph
#'
#' @param map Function map.
#' @return Data frame, one row for each function.

node_data_frame <- function(map) {
  df <- data.frame(
    stringsAsFactors = FALSE,
    ID = unique(c(names(map$data), unlist(map$data)))
  )
  df$exported <- df$ID %in% map$exports
  df
}

#' Extract edge (= function call) metadata from the call graph
#'
#' @param map Function map.
#' @return Data frame, one row for each edge.
#'
#' @importFrom stats aggregate

edge_data_frame <- function(map) {

  aggregate(
    weight ~ .,
    data.frame(
      stringsAsFactors = FALSE,
      from = rep(names(map$data), vapply(map$data, length, 1L)),
      to = unlist(map$data),
      weight = 1
    ),
    length
  )
}
