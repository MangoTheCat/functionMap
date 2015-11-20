
context("Parsing R scripts")

test_that("all functions are found", {
  src <- "
    x <- NULL
    f <- function() TRUE
    g <- function() FALSE
    h <- function() {
      k <- function() FALSE
      k()
    }
    y <- 1:10"

  expect_equal(
    names(parse_r_script(textConnection(src))),
    c("f", "g", "h")
  )
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
    parse_r_script(textConnection(src)),
    list(
      f = character(),
      g = "foo",
      h = c("bar", "g")
    )
  )
})

test_that("calls within do.call are found", {
  src <- "
    f <- function() do.call('blah', list())
  "

  expect_equal(
    parse_r_script(textConnection(src)),
    list(f = 'blah')
  )
})

test_that("locally defined functions are ignored", {
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
    parse_r_script(textConnection(src)),
    list(
      f = character(),
      g = "foo",
      h = c("bar", "g")
    )
  )
})

test_that("functions passed as arguments are ignored", {
  src <- "
    h <- function(k) {
      bar()
    }
  "

  expect_equal(
    parse_r_script(textConnection(src)),
    list(h = "bar")
  )

})

test_that("empty file is fine", {

  expect_equal(
    parse_r_script(textConnection("")),
    structure(list(), names = character())
  )

})

test_that("a file with errors is fine", {

  src <- "
    f <- function() foo()
    g <- this-is-an-error-here
    h <- function() bar()
  "

  expect_equal(
    suppressWarnings(parse_r_script(textConnection(src))),
    list(f = "foo")
  )

})

test_that("a file with a syntax error creates a warning", {

  src <- "
    f <- function() foo()
    g <- this-is-an-error-here
    h <- function() bar()
  "

  expect_warning(
    parse_r_script(textConnection(src)),
    "object 'this' not found"
  )
})

test_that("find functions in do.call and external calls", {

  src <- "
    f <- function() {
      foo()
      do.call('bar', list())
      .Call('foobar')
    }"

  expect_equal(
    sort(parse_r_script(textConnection(src))$f),
    c("bar", "Call::foobar", "foo")
  )
})

test_that("call counts are OK", {

  src <- "
    f <- function() {
      foo()
      foo()
      bar()
      foobar()
      foobar()
    }"

  expect_equal(
    parse_r_script(textConnection(src)),
    list(f = c("bar", "foo", "foobar"))
  )

  expect_equal(
    parse_r_script(textConnection(src), multiples = TRUE),
    list(f = c("foo", "foo", "bar", "foobar", "foobar"))
  )
})
