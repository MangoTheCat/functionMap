
context("parse_expression")

test_columns <- c("var", "line1", "col1", "line2", "col2")

test_that("simple global calls", {

  code <- "
    f <- function() {}
    g()
    f()
  "

  g <- map_expression(parse_expression(text = code))$calls
  expect_equal(g$var, "g")
  expect_equal(g$line1, 3)

  code <- '
    f <- function() {
      utils::untar("foobar")
      stats::mad(1:10)
    }
  '

  g <- map_expression(parse_expression(text = code))$calls
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
    map_expression(parse_expression(text = src))$calls[, test_columns],
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
    map_expression(parse_expression(text = src))$calls[, test_columns],
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
    map_expression(parse_expression(parsed = parsed))$calls[, test_columns],
    df(var = "bar", line1 = 4, col1 = 7, line2 = 4, col2 = 9)
  )

})

test_that("finding function names", {

  src1 <- "f <- function(k) { }"
  expect_equal(
    map_expression(parse_expression(text = src1))$funcs$var,
    c("_", "f")
  )

  src2 <- "f = function() { }"
  expect_equal(
    map_expression(parse_expression(text = src2))$funcs$var,
    c("_", "f")
  )
})
