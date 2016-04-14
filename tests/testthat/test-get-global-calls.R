
context("All global calls")


test_that("including base functions", {

  src <- '
    f <- function() {
      utils::untar("foobar")
      stats::mad(1:10)
    }
  '
  f <- with_src(src, source(file = src, keep.source = TRUE))$value

  gc <- get_global_calls("f", "f", f)

  expect_equal(
    gc,
    data_frame(
      to = c("{", "::", "::", ":", "utils::untar", "stats::mad"),
      type = "call",
      line = c(2, rep(NA_integer_, 5)),
      col1 = c(21, rep(NA_integer_, 5)),
      col2 = c(21, rep(NA_integer_, 5))
    )
  )
})


test_that("excluding base functions", {

  src <- '
    f <- function() {
      utils::untar("foobar")
      stats::mad(1:10)
    }
  '
  f <- with_src(src, source(file = src, keep.source = TRUE))$value

  gc <- get_global_calls("f", "f", f, include_base = FALSE)
  rownames(gc) <- NULL

  expect_equal(
    gc,
    data_frame(
      to = c("utils::untar", "stats::mad"),
      type = "call",
      line = NA_integer_,
      col1 = NA_integer_,
      col2 = NA_integer_
    )
  )
})
