
context("function maps api")

get_map <- function() {
  src <- "
    f <- function(foo, bar) {
      g()
    }
    g <- function(foobar) {
      h()
      utils::untar(foobar)
    }
    h <- function() {
      print('hello')
    }
  "
  map_r_script(textConnection(src))
}


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
      weight = 1
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


test_that("deps", {
  map <- get_map()
  expect_equal(
    deps(map),
    list(f = "g", g = c("h", "utils::untar"), h = character())
  )
})


test_that("rev_deps", {
  map <- get_map()
  expect_equal(
    rev_deps(map),
    list(f = character(), g = "f", h = "g")
  )
})
