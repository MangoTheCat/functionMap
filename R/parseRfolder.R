
#' Parse one or more folders of R scripts
#'
#' @param rpath Character vector of folders and files.
#'   Files are taken as given, folders are listed for files
#'   matching `rfilepattern`.
#' @param rfilepattern Pattern of file names to analyse. By
#'   default files with `.R` and `.r` extensions are used.
#' @param include_base Whether to include functions from the
#'   `base` package.
#' @return A named list with one entry for each analyzed functions.
#'   Each entry contains the names of the functions called.

parse_r_folder <- function(rpath, rfilepattern = "\\.[R|r]$",
                           include_base = FALSE) {

  rpath <- as.character(rpath)

  files <- lapply(
    rpath,
    function(rp) {
      if (file.info(rp)$isdir) {
        list.files(rp, full.names = TRUE, pattern = rfilepattern)
      } else {
        rp
      }
    }
  )

  files <- unique(unlist(files))

  res <- lapply(files, parse_r_script, include_base = include_base)

  do.call(c, res)
}

#' s4.source.from.rpackage
#'
#' Extract source code from S4 definitions from a R package
#'
#' @param base.path path to the R source, assume it to be a package base path
#' @param rfilepattern some author may use extension ".[qQ][sS]" other than [Rr], this option can select from those
#' @return network object
s4.source.from.rpackage <- function(base.path, rfilepattern = '\\.[Rr]$'){
    # s4.list <- guess.s4.from.dir(base.path)
    # if (is.null(s4.list) || is.null(s4.list$S4.methods)) return(NULL)
    # If it's a installed package , parseS4fromNs will work well
    s4.defn <- try(dumpS4Generic(basename(base.path), style='S4'), silent=TRUE)
    # If not, we may have to extract.S4.defn from source code, install them in .GlobalEnv, and dump again
    if (is(s4.defn, 'try-error')) {
        txt <- try(extract.S4.defn(srclist= list.files(file.path(base.path,'R'), pattern=rfilepattern, full.names=TRUE, recursive=TRUE)),
                silent=TRUE)
        if (is(txt, 'try-error')) {
            stop('Failed to extract S4 definition from this package!')
        }
        try(eval(parse(text=txt)), silent=TRUE)
        # if above is success, those definitions are exported to .GlobalEnv and we can use parseS4fromNs
        s4.defn <- try(dumpS4Generic(style='S4'), silent=TRUE)
        if (is(s4.defn, 'try-error')) {
            stop('Failed to extract S4 definition from this package!')
        }
    }
    s4.defn
}
