
context("double colon (::) calls")

test_that(":: calls are found, only calls", {

  fobj <- function()
  {
    pkg::func("foobar", 1:5)
    pkg::func(pkg::func(1:10, "foo"))
    pkg::func2
  }

  f <- with_src(
    paste(deparse(fobj), collapse = "\n"),
    get_funcs_from_r_script(src)
  )[[1]]

  expect_equal(
    double_colon_calls(f),
    data_frame(
      to = "pkg::func",
      type = "call",
      line = c(3, 4, 4),
      col1 = c(5, 5, 15),
      col2 = c(13, 13, 23)
    )
  )
})


test_that(":: calls are not prefixed twice with pkg name", {

  map <- map_r_package("testdc")

  expect_equal(
    get_graph(map, only_me = FALSE),
    list(
      "f" = c("pkg::func1", "pkg::func2", "pkg::func1"),
      "pkg::func1" = character(),
      "pkg::func2" = character()
    )
  )
})
