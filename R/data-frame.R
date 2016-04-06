
#' Extract vertex (= function) metadata from the call graph
#'
#' @param data Parsed function call data.
#' @param exports Character vector of exported functions.
#' @return Data frame, one row for each function.
#' @keywords internal

node_data_frame <- function(data, exports) {
  df <- data.frame(
    stringsAsFactors = FALSE,
    ID = unique(c(names(data), unlist(data)))
  )
  df$own <- df$ID %in% names(data)
  df$exported <- df$ID %in% exports
  df
}

#' Extract edge (= function call) metadata from the call graph
#'
#' @param map Function map.
#' @return Data frame, one row for each edge.
#'
#' @importFrom stats aggregate
#' @keywords internal

edge_data_frame <- function(data) {

  aggregate(
    weight ~ .,
    data.frame(
      stringsAsFactors = FALSE,
      from = rep(names(data), vapply(data, length, 1L)),
      to = unlist(data),
      weight = 1
    ),
    length
  )
}
