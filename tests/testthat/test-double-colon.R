
context("double colon (::) calls")

test_that(":: calls are found, only calls", {

  f <- function() {
    pkg::func("foobar", 1:5)
    pkg::func(pkg::func(1:10, "foo"))
    pkg::func2
  }

  cc <- double_colon_calls(f)
  expect_equal(cc, rep("pkg::func", 3))
})


test_that(":: calls are not prefixed twice with pkg name", {

  map <- map_r_package("testdc")

  expect_equal(
    get_graph(map, only_me = FALSE),
    list(
      "f" = c("pkg::func1", "pkg::func2"),
      "pkg::func1" = character(),
      "pkg::func2" = character()
    )
  )
})
