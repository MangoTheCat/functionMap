
context("S3 generics")

test_that("", {

  env <- new.env()
  env$f <- function() UseMethod("f")
  env$f.default <- function() "default"
  env$f.foo <- function() "foo"
  env$g <- function() UseMethod("g")
  env$g.foo <- function() "gfoo"
  env$g.blah <- function() "blah"

  expect_equal(
    s3_calls("f", envir = env),
    c("f.default", "f.foo")
  )

})
