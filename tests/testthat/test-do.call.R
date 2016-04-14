
context("do.call expressions")

df <- function(to, type = "call") {
  data_frame(to = to, type = type, line = NA_integer_,
             col1 = NA_integer_, col2 = NA_integer_)
}

test_that("we can find globals in do.call calls", {

  ## y is local to the function, so we only want to find foo
  f <- function(x) {
    y <- function() TRUE
    do.call(y, list())
    do.call("foo", list())
  }

  expect_equal(func_arg_globals(f), df(to = "foo"))

  ## Find foo only, we don't report non-string arguments to do.call,
  ## because those are found by findGlobals() anyway
  f <- function() {
    do.call(y, list())
    do.call("foo", list())
  }

  expect_equal(sort(func_arg_globals(f)$to), c("foo", "y"))

  ## The original test cases
  expect_equal(
    func_arg_globals(function() do.call("sin", list(1:10)))$to,
    "sin"
  )

  expect_equal(
    func_arg_globals(function() do.call(myfunction, list(1:10)))$to,
    "myfunction"
  )

  expect_equal(
    func_arg_globals(function() do.call(fun2, list(1:10)))$to,
    "fun2"
  )
})


test_that("multiple calls are found multiple times", {

  f <- function() {
    do.call(y, list())
    do.call(y, list())
    do.call(y, list())
  }

  expect_equal(func_arg_globals(f)$to, rep("y", 3))

  f <- function() {
    do.call("y", list())
    do.call("y", list())
    do.call("y", list())
  }

  expect_equal(func_arg_globals(f)$to, c("y", "y", "y"))
})


test_that("mixed symbols and strings work", {

  f <- function() {
    z <- 10
    do.call(y, list())
    do.call(y, list())
    do.call("y", list())
    do.call("y", list())
  }

  expect_equal(func_arg_globals(f)$to, c("y", "y", "y", "y"))

})


test_that("do.call in default argument is picked up", {

  f <- function() {
    g <- function(x = do.call("xx", list())) { }
    do.call("yy", list())
  }

  expect_equal(func_arg_globals(f)$to, c("xx", "yy"))

})
