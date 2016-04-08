
skip_without_package <- function(pkg, msg = NULL) {

  if (is.null(msg)) msg <- paste("Need package", pkg)

  if (pkg %in% loadedNamespaces()) return()

  on.exit(try(unloadNamespace(pkg), silent = TRUE), add = TRUE)
  if (requireNamespace(pkg, quietly = TRUE)) return()

  skip(msg)
}

skip_if_offline <- function(host = "httpbin.org", port = 80) {

  res <- tryCatch(
    pingr::ping_port(host, count = 1L, port = port),
    error = function(e) NA
  )

  if (is.na(res)) skip("No internet connection")
}
