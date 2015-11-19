
in_list <- function(elem, list) {
  for (e in list) if (identical(elem, e)) return(TRUE)
  FALSE
}

match_list <- function(elem, list) {
  for (i in seq_along(list)) if (identical(elem, list[[i]])) return(i)
  NA_integer_
}

#' Drop NULL elements from a list
#' @param x input list

drop_null <- function(x) {
  Filter(function(xx) !is.null(xx), x)
}
