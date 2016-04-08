
context("Mapping CRAN packages")

test_that("map_cran_package", {

  skip_if_offline()
  skip_on_cran()

  map <- map_cran_package("dotenv", quiet = TRUE)

  expect_true(is(map, "function_map"))
  expect_true("load_dot_env" %in% functions(map))
})
