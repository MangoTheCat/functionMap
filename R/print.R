
mark_exported <- function(names, exp) {
  ifelse(names %in% exp, paste0(names, "*"), names)
}

print_graph <- function(x, ...) {

  data <- x$data
  data <- data[ sort(names(data)) ]

  if (!is.null(x$exports)) {
    for (i in seq_along(data)) {
      data[[i]] <- mark_exported(data[[i]], x$exports)
    }
    names(data) <- mark_exported(names(data), x$exports)
  }

  lapply(names(data), function(n) {
    cat(" ", tail_style(n), "\n", sep = "")

    funcs <- head_style(sort(unique(data[[n]])))
    funcs <- paste(funcs, collapse = ", ")
    funcs <- strwrap(paste0(arrow(), " ", funcs), indent = 3, exdent = 6)
    funcs <- paste(funcs, collapse = "\n")

    cat(funcs, "\n", sep = "")
  })
}

fill_line <- function(x, chr = "-", width = getOption("width", 80)) {
  len <- width - nchar(x, type = "width") - 4
  if (len <= 0) {
    x
  } else {
    paste0(
      paste0(rep(chr, len), collapse = ""),
      " ", x,
      " --"
    )
  }
}

#' @importFrom crayon green bold

header_style <- function(x) {
  bold(green(fill_line(x, "-")))
}

#' @importFrom crayon green

tail_style <- function(x) {
  green(x)
}

#' @importFrom crayon blue yellow

head_style <- function(x) {
  vapply(
    x, "",
    FUN = function(xx) {
      if (grepl("^\\..*::", xx)) {
        yellow(xx)
      } else if (grepl("::", xx)) {
        blue(xx)
      } else {
        xx
      }
    }
  )
}

#' @importFrom crayon yellow

arrow <- function(x) {
  yellow("->")
}

#' @method print function_map
#' @export

print.function_map <- function(x, ...) {

  cat(header_style("Function map"), "\n", sep = "")

  print_graph(x, ...)

  invisible(x)
}

#' @method print function_map_rfile
#' @export

print.function_map_rfile <- function(x, ...) {

  head <- paste0("Map of R script '", x$rfile, "'")
  cat(header_style(head), "\n", sep = "")

  print_graph(x, ...)

  invisible(x)
}

#' @method print function_map_rfolder
#' @export

print.function_map_rfolder <- function(x, ...) {

  head <- paste0("Map of R folder '", x$rpath, "'")
  cat(header_style(head), "\n", sep = "")

  print_graph(x, ...)

  invisible(x)
}

#' @method print function_map_rpackage
#' @export

print.function_map_rpackage <- function(x, ...) {

  head <- paste0("Map of R package '", x$package, "'")
  cat(header_style(head), "\n", sep = "")

  print_graph(x, ...)

  invisible(x)

}
