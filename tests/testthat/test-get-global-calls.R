
context("All global calls")


test_that("including base functions", {

  f <- function() {
    utils::untar("foobar")
    stats::mad(1:10)
  }

  gc <- get_global_calls("f", f)

  expect_equal(
    gc,
    data_frame(
      to = c(":", "::", "{", "utils::untar", "stats::mad"),
      type = "call"
    )
  )
})


test_that("excluding base functions", {

  f <- function() {
    utils::untar("foobar")
    stats::mad(1:10)
  }

  gc <- get_global_calls("f", f, include_base = FALSE)
  rownames(gc) <- NULL

  expect_equal(
    gc,
    data_frame(
      to = c("utils::untar", "stats::mad"),
      type = "call"
    )
  )
})
