
context("External calls")

test_that("string external calls are found", {

  f <- function() {
    .C("foo")
    .Call("bar")
    .Fortran("foobar")
    .External("exfoobar")
  }

  expect_equal(
    external_calls(f),
    c(".C::foo", ".Call::bar", ".Fortran::foobar", ".External::exfoobar")
  )
})

test_that("external calls in arguments to external calls", {

  f <- function() {
    .C("foo", .Call("bar", .Fortran("foobar")))
  }

  expect_equal(
    external_calls(f),
    c(".C::foo", ".Call::bar", ".Fortran::foobar")
  )
})

test_that("external calls are counted properly", {

  f <- function() {
    .C("foo")
    .C("foo")
    .Fortran("bar")
    .External("foobar")
  }

  expect_equal(
    external_calls(f),
    c(".C::foo", ".Fortran::bar", ".External::foobar")
  )

  expect_equal(
    external_calls(f, multiples = TRUE),
    c(".C::foo", ".C::foo", ".Fortran::bar", ".External::foobar")
  )

})

test_that("external call in default argument is picked up", {

  f <- function() {
    g <- function(x = .Call("yy")) { }
  }

  expect_equal(external_calls(f), ".Call::yy")
})
