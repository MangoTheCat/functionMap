
get_base_funcs <- function() {
  ls(asNamespace("base"), all.names = TRUE)
}

remove_base_functions <- function(funcs) {

  base_funcs <- get_base_funcs()

  if (is.list(funcs)) {
    lapply(
      funcs,
      function(x) { x[ ! x %in% base_funcs ] }
    )
  } else {
    funcs[ ! funcs %in% base_funcs ]
  }
}
