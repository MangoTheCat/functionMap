
print_graph <- function(x, ...) {

  data <- x$data
  data <- data[ sort(names(data)) ]

  lapply(names(data), function(n) {
    cat(n, ":\n", sep = "")

    funcs <- paste(sort(unique(data[[n]])), collapse = ", ")
    funcs <- strwrap(paste0("-> ", funcs), indent = 2, exdent = 5)
    funcs <- paste(funcs, collapse = "\n")

    cat(funcs, "\n", sep = "")
  })
}

#' @method print function_map
#' @export

print.function_map <- function(x, ...) {

  cat("Function map\n")

  print_graph(x, ...)

  invisible(x)
}

#' @method print function_map_rfile
#' @export

print.function_map_rfile <- function(x, ...) {

  cat("Function map for R script '", x$rfile, "':\n", sep = "")

  print_graph(x, ...)

  invisible(x)
}

#' @method print function_map_rfolder
#' @export

print.function_map_rfolder <- function(x, ...) {

  cat("Function map for R folder '", x$rpath, "':\n", sep = "")

  print_graph(x, ...)

  invisible(x)
}
