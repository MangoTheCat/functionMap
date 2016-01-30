
#' Parse one or more folders of R scripts
#'
#' @param rpath Character vector of folders and files.
#'   Files are taken as given, folders are listed for files
#'   matching `rfilepattern`.
#' @param rfilepattern Pattern of file names to analyse. By
#'   default files with `.R`, `.r`, `.S` and `.s` extensions are used.
#' @param include_base Whether to include functions from the
#'   `base` package.
#' @param multiples Whether to include functions as many times as
#'   they are called.
#' @param env Environment to parse the files into. If `NULL`, then
#'   a separate temporary environment is used for each file.
#' @return A named list with one entry for each analyzed functions.
#'   Each entry contains the names of the functions called.
#'
#' @keywords internal

parse_r_folder <- function(rpath, rfilepattern = default_r_file_pattern(),
                           include_base = FALSE, multiples = FALSE,
                           env = NULL) {

  rpath <- as.character(rpath)

  files <- lapply(
    rpath,
    function(rp) {
      if (!file.exists(rp)) stop("File does not exist: ", rp)
      if (file.info(rp)$isdir) {
        list.files(rp, full.names = TRUE, pattern = rfilepattern)
      } else {
        rp
      }
    }
  )

  files <- unique(unlist(files))

  res <- lapply(
    files,
    parse_r_script,
    include_base = include_base,
    multiples = multiples,
    env = env
  )

  do.call(c, res)
}
