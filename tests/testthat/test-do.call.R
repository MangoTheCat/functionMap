
context("do.call expressions")

test_that("we can find globals in do.call calls", {

  ## y is local to the function, so we only want to find foo
  f <- function(x) {
    y <- function() TRUE
    do.call(y, list())
    do.call("foo", list())    
  }

  expect_equal(do_call_globals(f), "foo")

  ## Find foo only, we don't report non-string arguments to do.call,
  ## because those are found by findGlobals() anyway
  f <- function() {
    do.call(y, list())
    do.call("foo", list())
  }

  expect_equal(sort(do_call_globals(f)), c("foo", "y"))

  ## The original test cases
  expect_equal(
    do_call_globals(function() do.call("sin", list(1:10))),
    "sin"
  )

  expect_equal(
    do_call_globals(function() do.call(myfunction, list(1:10))),
    "myfunction"
  )

  expect_equal(
    do_call_globals(function() do.call(fun2, list(1:10))),
    "fun2"
  )
})
