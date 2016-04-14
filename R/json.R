
#' Export a map to a JSON file
#'
#' @param map The function map.
#' @param file The file or connection to write to.
#' @param pretty Whether to pretty print the JSON file.
#'
#' @export
#' @importFrom jsonlite toJSON unbox

export_map <- function(map, file, pretty = TRUE) {
  data <- list(
    package = unbox(map$package),
    exports = map$exports,
    functions = node_df(map),
    calls = edge_df(map)
  )

  cat(toJSON(data, pretty = pretty), file = file)
}
