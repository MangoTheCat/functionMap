
context("Creating maps")


test_that("map_r_script", {
  src <- "
    f <- function(foo, bar) {
      f()
      g()
    }
    g <- function(foobar) {
      h()
      utils::untar(foobar)
    }
    h <- function() {
      print('hello')
    }
    iso <- function() {
      ## This is never called
    }
  "

  map <- map_r_script(textConnection(src))
  expect_equal(class(map), c("function_map_rfile", "function_map"))
  expect_equal(
    deps(map),
    list(
      f = c("f", "g"),
      g = c("h", "utils::untar"),
      h = character(),
      iso = character()
    )
  )
})


test_that("map_r_folder", {
  map <- map_r_folder("testfolder")
  expect_equal(class(map), c("function_map_rfolder", "function_map"))
  expect_equal(
    deps(map),
    list(
      f = c("f", "g"),
      g = c("h", "utils::untar"),
      h = character(),
      iso = character(),
      k = "x",
      y = "f"
    )
  )
})
