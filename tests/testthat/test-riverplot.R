
context("River (Sankey) plot")

test_that("x positions are calculated properly", {

  src = "
    f <- function() { g() }
    g <- function() { h() }
    h <- function() {     }
  "

  map <- map_r_script(textConnection(src))

  expect_equal(
    calculate_x_pos(map),
    c(f = 0, g = 1, h = 2)
  )

  src = "
    f <- function() { foobar(); bar() }
    g <- function() { bar() }
    h <- function() {  }
  "

  map <- map_r_script(textConnection(src))

  expect_equal(
    calculate_x_pos(map),
    c(f = 0, g = 0, h = 0, foobar = 1, bar = 1)
  )
})
