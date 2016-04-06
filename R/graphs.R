
#' Convert a function map to a directed graph
#'
#' The graph is represented as an adjacency list. It is a named
#' list of character vectors, and a character vector contains
#' the successors of the vertex.
#'
#' @param map The function map.
#' @param only_me Whether to include only the functions under study
#'   (i.e. the functions defined in the script(s) or the package),
#'   or all functions called by them as well.
#' @return The graph as an adjacency list.
#' @keywords internal

get_graph <- function(map, only_me = TRUE) {

  nodes <- node_df(map)
  edges <- edge_df(map)

  if (only_me) {
    own <- nodes$ID[ nodes$own ]
    nodes <- nodes[ nodes$ID %in% own, ]
    edges <- edges[ edges$to %in% own, ]
  }

  modifyList(
    structure(
      replicate(nrow(nodes), character()),
      names = nodes[[1]]
    ),
    tapply(edges[,2], edges[,1], c, simplify = FALSE)
  )
}

#' Change the direction of each edge to the opposite in a graph
#'
#' I.e. from an in-adjacency list create an out-adjacency list,
#' and vice versa.
#'
#' @param graph The input graph, an adjacency list.
#' @return The output graph
#' @keywords internal

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

#' Vertices that cannot be found from the a set of source vertices
#'
#' @param graph The input graph, an adjacency list.
#' @param sources The names of the source vertices.
#' @return A character vector of unreachable vertex names.
#' @keywords internal

isolates <- function(graph, sources) {

  ## Do a BFS from the exports, and anything that is not included
  ## is never called (Well, most probably.)
  reachable <- bfs(graph, sources)

  setdiff(names(graph), reachable)
}

#' Perform a breadth first search (BFS) on a graph
#'
#' Perform a BFS on a graph, from a set of seed vertices.
#' Once all vertices reachable from the seeds are visited,
#' the search terminates.
#'
#' @param graph The input graph, an adjacency list.
#' @param seeds The seed vertices, a character vector.
#' @return A character vector of vertex names, the visited vertices
#'   in the order of their visit.
#' @keywords internal

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

#' Topological sort of a graph
#'
#' @param graph Input graph as an adjacency list.
#' @return Character vector of vertex names in
#'   topological order.
#' @keywords internal

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

#' Remove loops from a graph
#'
#' @param graph Input graph, as an adjacency list.
#' @return Another graph, with the loop edges removed.
#' @keywords internal

remove_loops <- function(graph) {
  structure(
    lapply(names(graph), function(n) setdiff(graph[[n]], n)),
    names = names(graph)
  )
}
