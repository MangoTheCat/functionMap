
#' Extract vertex (= function) metadata from the call graph
#'
#' @param data Parsed function call data.
#' @param exports Character vector of exported functions.
#' @return Data frame, one row for each function.
#' @keywords internal

node_data_frame <- function(data, exports) {
  callers <- names(data)
  callees <- setdiff(unique(unlist(lapply(data, "[[", "to"))), callers)

  files <- c(
    vapply(data, function(x) as.character(attr(x, "pos")$file), ""),
    rep(NA_character_, length(callees))
  )
  lines <- c(
    vapply(data, function(x) as.integer(attr(x, "pos")$line), 1L),
    rep(NA_character_, length(callees))
  )

  df <- data.frame(
    stringsAsFactors = FALSE,
    ID = c(callers, callees),
    file = files,
    line = lines
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

  do.call(rbind, dfs)
}
