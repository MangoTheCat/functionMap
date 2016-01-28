
context("Parsing R packages")

test_that("package sources are evaluated in a single env", {

  map <- map_r_package("testEnv")

  expect_equal(
    map$data,
    list(f = "f", g = "f")
  )
})

test_that("package sources use Collate from DESCRIPTION", {

  map <- map_r_package("testCollate")

  expect_equal(
    map$data,
    list(f = "f", g = "f")
  )
})
