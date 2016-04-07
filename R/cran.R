
#' Create the map of a CRAN package
#'
#' The latest version of the package is used.
#'
#' @param package CRAN package name.
#' 
#' @importFrom utils download.packages untar
#' @export
#' @examples
#' \dontrun{
#' map <- map_cran_package("dotenv")
#' map
#' sankey_plot(map)
#' }

map_cran_package <- function(package, version = NULL) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)

  file <- download.packages(
    package,
    destdir = tmp,
    type = "source",
    quiet = TRUE
  )
  untar(file[,2], exdir = tmp)

  pkgdir <- file.path(tmp, package)
  map_r_package(pkgdir)
}
