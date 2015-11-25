
node_data_frame <- function(map) {
  data.frame(
    stringsAsFactors = FALSE,
    ID = unique(c(names(map$data), unlist(map$data)))
  )
}

#' @importFrom stats aggregate

edge_data_frame <- function(map) {

  aggregate(
    Value ~ .,
    data.frame(
      stringsAsFactors = FALSE,
      N1 = rep(names(map$data), vapply(map$data, length, 1L)),
      N2 = unlist(map$data),
      Value = 1
    ),
    length
  )
}
