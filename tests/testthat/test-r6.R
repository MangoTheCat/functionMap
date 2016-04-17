
context("R6 classes")

test_that("No warnings for R6", {

  expect_silent(
    map <- map_r_package("testr6")
  )

})
