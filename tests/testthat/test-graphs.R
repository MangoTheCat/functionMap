
context("Graphs")


test_that("get_graph", {
  map <- get_map()

  expect_equal(
    get_graph(map),
    list(f = "g", g = "h", h = character())
  )

  expect_equal(
    get_graph(map, only_me = FALSE),
    list(
      f = "g",
      g = c("h", "utils::untar"),
      h = character(),
      `utils::untar` = character()
    )
  )
})


test_that("twist_graph", {
  map <- get_map()
  g <- get_graph(map)
  expect_equal(
    twist_graph(g),
    list(f = character(), g = "f", h = "g")
  )
})


test_that("isolates", {
  map <- get_map2()
  g <- get_graph(map)

  expect_equal(isolates(g, "f"), "iso")
  expect_equal(isolates(g, "g"), c("f", "iso"))
  expect_equal(isolates(g, "h"), c("f", "g", "iso"))
  expect_equal(isolates(g, "iso"), c("f", "g", "h"))

  expect_equal(isolates(g, c("f", "iso")), character())
})


test_that("bfs", {
  map <- get_map2()
  g <- get_graph(map)

  expect_equal(bfs(g, "f"), c("f", "g", "h"))
  expect_equal(bfs(g, c("f", "iso")), c("f", "iso", "g", "h"))
  expect_equal(bfs(g, character()), character())
})


test_that("topo_sort", {
  g <- get_graph(get_map())
  g2 <- get_graph(get_map2())

  expect_equal(topo_sort(g), c("f", "g", "h"))
  expect_error(topo_sort(g2), "Call graph not a DAG")
})


test_that("remove_loops", {
  g <- get_graph(get_map2())
  expect_equal(
    remove_loops(g),
    list(f = "g", g = "h", h = character(), iso = character())
  )
})
