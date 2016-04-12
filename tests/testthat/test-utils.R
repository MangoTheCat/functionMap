
context("Utility functions")

test_that("drop_null", {

  expect_equal(drop_null(list(1,2,3)), list(1, 2, 3))
  expect_equal(drop_null(list(1, NULL, 2)), list(1, 2))
  expect_equal(drop_null(list(NULL, 1)), list(1))
  expect_equal(drop_null(list(1, NULL)), list(1))
  expect_equal(drop_null(list(NULL)), list())
  expect_equal(drop_null(list(NULL, NULL)), list())
  expect_equal(drop_null(list()), list())

})
