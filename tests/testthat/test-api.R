
context("function maps api")

test_that("node_df", {
  map <- get_map()
  expect_equal(
    node_df(map),
    data_frame(
      ID = c("f", "g", "h", "utils::untar"),
      own = c(TRUE, TRUE, TRUE, FALSE),
      exported = FALSE
    )
  )
})


test_that("edge_df", {
  map <- get_map()
  expect_equal(
    edge_df(map),
    data_frame(
      from = c("f", "g", "g"),
      to = c("g", "h", "utils::untar"),
      type = "call",
      line = c(3, 6, 7),
      col1 = c(7, 7, 7),
      col2 = c(7, 7, 18)
    )
  )
})


test_that("functions", {
  map <- get_map()
  expect_equal(functions(map), c("f", "g", "h"))
})


test_that("functions_called", {
  map <- get_map()
  expect_equal(functions_called(map), c("g", "h", "utils::untar"))
})


test_that("unused_functions", {
  map <- map_r_package("testEnv")
  expect_equal(unused_functions(map), character())

  map <- map_r_package("testpkg")
  expect_equal(unused_functions(map), c("iso", "k", "y"))

  map <- get_map2()
  expect_error(unused_functions(map), "only works for R packages")
})


test_that("deps", {
  map <- get_map()
  expect_equal(
    deps(map),
    list(f = "g", g = c("h", "utils::untar"), h = character())
  )
  map <- get_map3()
  expect_equal(
    deps(map, multiples = TRUE),
    list(
      f = c("f", "g", "g"),
      g = c("h", "h", "utils::untar"),
      h = character(),
      iso = character()
    )
  )
})


test_that("rev_deps", {
  map <- get_map()
  expect_equal(
    rev_deps(map),
    list(f = character(), g = "f", h = "g")
  )
  map <- get_map3()
  expect_equal(
    rev_deps(map, multiples = TRUE),
    list(
      f = "f",
      g = c("f", "f"),
      h = c("g", "g"),
      iso = character()
    )
  )
})
