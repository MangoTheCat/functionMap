
context("Parsing R scripts")

df <- function(to, type) {
  data_frame(to = to, type = type, line = NA_integer_,
             col1 = NA_integer_, col2 = NA_integer_)
}

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
    names(with_src(src, parse_r_script(src))),
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
    with_src(src, parse_r_script(src)),
    list(
      f = df(to = character(), type = character()),
      g = df(to = "foo", type = "call"),
      h = df(to = c("bar", "g"), type = "call")
    )
  )
})

test_that("calls within do.call are found", {
  src <- "
    f <- function() do.call('blah', list())
  "

  expect_equal(
    with_src(src, parse_r_script(src)),
    list(f = df(to = "blah", type = "call"))
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
    with_src(src, parse_r_script(src)),
    list(
      f = df(to = character(), type = character()),
      g = df(to = "foo", type = "call"),
      h = df(to = c("bar", "g"), type = "call")
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
    with_src(src, parse_r_script(src)),
    list(h = data_frame(to = "bar", type = "call", line = 3, col1 = 7, col2 = 9))
  )

})

test_that("empty file is fine", {

  expect_equal(
    with_src("", parse_r_script(src)),
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
    suppressWarnings(with_src(src, parse_r_script(src))),
    list(
      f = data_frame(to = "foo", type = "call", line = 2, col1 = 21, col2 = 23),
      h = df(to = "bar", type = "call")
    )
  )

})

test_that("a file with a syntax error creates a warning", {

  src <- "
    f <- function() foo()
    g <- this-is-an-error-here
    h <- function() bar()
  "

  expect_warning(
    with_src(src, parse_r_script(src)),
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
    sort(with_src(src, parse_r_script(src))$f$to),
    sort(c(".Call::foobar", "bar", "foo"))
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
    with_src(src, parse_r_script(src)),
    list(
      f = data_frame(
        to = c("foo", "foo", "bar", "foobar", "foobar"),
        type = "call",
        line = c(3, 4, 5, 6, 7),
        col1 = 7,
        col2 = c(9, 9, 9, 12, 12)
      )
    )
  )
})

test_that("The same env is used for the whole file", {

  src <- "
    f <- function() TRUE
    g <- f
    h <- function() g()
    y <- h"

  p <- with_src(src, parse_r_script(src))

  e <- list(
    f = df(to = character(), type = character()),
    g = df(to = character(), type = character()),
    h = data_frame(to = "g", type = "call", line = 4, col1 = 21, col2 = 21),
    y = df(to = "g", type = "call")
  )

  expect_equal(p, e)
})

test_that("Non-function code works", {

  src <- "
    ls()
    lm(1:10 ~ runif(10))
    x <- function() { ls() }
    x()"

  expect_equal(
    with_src(src, parse_r_script(src)),
    list(
      x = df(to = character(), type = character()),
      "_" = df(to = c("lm", "x"), type = "call")
    )
  )

})

test_that("More complex non-function code works", {

  src <- "
    description <- R6Class(
      'description',
      public = list(
        initialize = function(cmd = NULL, file = NULL, text = NULL,
                              package = NULL)
          desc_create(self, private, cmd, file, text, package),
        write = function(file = NULL, normalize = FALSE)
          desc_write(self, private, file, normalize),
        get_maintainer = function()
          desc_get_maintainer(self, private)
     ),
     private = list(
       data = NULL,
       path = 'DESCRIPTION'
     )
   )"

  expect_warning(
    p <- with_src(src, parse_r_script(src)),
    "R6Class"
  )

  expect_equal(names(p), "_")

  expect_equal(
    sort(p[["_"]]$to),
    sort(c("desc_create", "desc_get_maintainer", "desc_write", "R6Class"))
  )
})
