
#' Extract vertex (= function) metadata from the call graph
#'
#' @param data Parsed function call data.
#' @param exports Character vector of exported functions.
#' @return Data frame, one row for each function.
#' @keywords internal

node_data_frame <- function(data, exports) {
  callers <- names(data)
  callees <- unlist(lapply(data, "[[", "to"))
  df <- data.frame(
    stringsAsFactors = FALSE,
    ID = unique(c(callers, callees))
  )
  df$own <- df$ID %in% callers
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

  callers <- names(data)
  dfs <- lapply(callers, function(caller) {
    data_frame(from = caller, data[[caller]])
  })

  df <- do.call(rbind, dfs)

  aggregate(weight ~., data = transform(df, weight = 1), length)
}
