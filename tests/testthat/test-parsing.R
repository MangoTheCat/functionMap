
context("Parsing R expressions")

test_that("syntax errors", {
  src <- "
    f <- function() { g() }
    h <- function-error()
    g <- function() { TRUE }
  "

  expect_warning(
    p <- parse_r_script(textConnection(src)),
    "textConnection.*unexpected"
  )

})
