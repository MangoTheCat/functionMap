
get_graph <- function(map, only_me = TRUE) {
  if (only_me) {
    ## Only keep my own functions
    V <- names(map$data)
    lapply(map$data, intersect, V)

  } else {
    V <- unique(c(names(map$data), unlist(map$data)))
    res <- replicate(length(V), character())
    names(res) <- V
    res[ names(map$data) ] <- map$data
    res
  }
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

topo_sort <- function(graph) {

  V <- names(graph)
  N <- length(V)

  ## some easy cases
  if (length(graph) <= 1 ||
      sum(sapply(graph, length)) == 0) return(V)

  marked <- 1L; temp_marked <- 2L; unmarked <- 3L
  marks <- structure(rep(unmarked, N), names = V)
  result <- character(N)
  result_ptr <- N

  visit <- function(n) {
    if (marks[n] == temp_marked) stop("Call graph not a DAG: ", n)
    if (marks[n] == unmarked) {
      marks[n] <<- temp_marked
      for (m in graph[[n]]) visit(m)
      marks[n] <<- marked
      result[result_ptr] <<- n
      result_ptr <<- result_ptr - 1
    }
  }

  while (any(marks == unmarked)) {
    visit(names(which(marks == unmarked))[1])
  }

  result
}

remove_loops <- function(graph) {
  structure(
    lapply(names(graph), function(n) setdiff(graph[[n]], n)),
    names = names(graph)
  )
}
