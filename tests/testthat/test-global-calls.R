
context("find global calls")

test_that("simple global calls", {

  code <- "
    f <- function() {}
    g()
    f()
  "

  g <- global_calls(text = code)
  expect_equal(g$var, "g")
  expect_equal(g$line1, 3)

  code <- '
    f <- function() {
      utils::untar("foobar")
      stats::mad(1:10)
    }
  '

  g <- global_calls(text = code)
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
    global_calls(text = src)[, c("var", "line1", "col1", "line2", "col2")],
    df(
      var = c("foo", "bar"),
      line1 = c(4, 8),
      col1 = c(21, 7),
      line2 = c(4, 8),
      col2 = c(23, 9)
    )
  )

})
