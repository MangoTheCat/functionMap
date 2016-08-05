
context("parse_expression")

test_columns <- c("var", "line1", "col1", "line2", "col2")

test_that("simple global calls", {

  code <- "
    f <- function() {}
    g()
    f()
  "

  g <- map_expression(text = code)
  expect_equal(g$var, "g")
  expect_equal(g$line1, 3)

  code <- '
    f <- function() {
      utils::untar("foobar")
      stats::mad(1:10)
    }
  '

  g <- map_expression(text = code)
  expect_equal(g$var, c("untar", "mad"))
  expect_equal(g$line1, 3:4)
  expect_equal(g$line2, 3:4)
})

test_that("function calls are found", {
  src <- "
    x <- NULL
    f <- function() TRUE
    g <- function() foo()
    h <- function() {
      k <- function() FALSE
      k()
      bar()
      g()
    }
    y <- 1:10"

  expect_equal(
    map_expression(text = src)[, test_columns],
    df(
      var = c("foo", "bar"),
      line1 = c(4, 8),
      col1 = c(21, 7),
      line2 = c(4, 8),
      col2 = c(23, 9)
    )
  )

})

test_that("functions passed as arguments are ignored", {
  src <- "
    h <- function(k) {
      k()
      bar()
    }
  "

  expect_equal(
    map_expression(text = src)[, test_columns],
    df(var = "bar", line1 = 4, col1 = 7, line2 = 4, col2 = 9)
  )
})

test_that("map_expression works with parsed data", {
  src <- "
    h <- function(k) {
      k()
      bar()
    }
  "

  parsed <- parse(text = src, keep.source = TRUE)

  expect_equal(
    map_expression(parsed = parsed)[, test_columns],
    df(var = "bar", line1 = 4, col1 = 7, line2 = 4, col2 = 9)
  )

})
