
get_graph <- function(map) {
  ## Only keep my own functions
  V <- names(map$data)
  lapply(map$data, intersect, V)
}

twist_graph <- function(graph) {
  res <- structure(
    replicate(length(graph), character()),
    names = names(graph)
  )

  for (v in names(graph)) {
    for (w in graph[[v]]) {
      res[[w]] <- c(res[[w]], v)
    }
  }

  res
}

isolates <- function(graph, sources) {

  ## Do a BFS from the exports, and anything that is not included
  ## is never called (Well, most probably.)
  reachable <- bfs(graph, sources)

  setdiff(names(graph), reachable)
}

bfs <- function(graph, seeds) {

  V <- names(graph)
  N <- length(V)
  reachable <- seeds
  marks <- structure(rep(FALSE, N), names = V)

  while (length(seeds)) {

    s <- seeds[1]
    seeds <- seeds[-1]

    for (n in graph[[s]]) {
      if (!marks[[n]]) {
        seeds <- c(seeds, n)
        reachable <- c(reachable, n)
        marks[[n]] <- TRUE
      }
    }
  }

  reachable
}
