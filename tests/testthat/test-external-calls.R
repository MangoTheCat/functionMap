
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
    c("C_foo", "Call_bar", "Fortran_foobar", "External_exfoobar")
  )
})

test_that("external calls in arguments to external calls", {

  f <- function() {
    .C("foo", .Call("bar", .Fortran("foobar")))
  }

  expect_equal(
    external_calls(f),
    c("C_foo", "Call_bar", "Fortran_foobar")
  )
})
