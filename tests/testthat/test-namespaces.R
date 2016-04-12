
context("Operations on namespaces")

test_that("reading a NAMESPACE file, imports", {
  imp <- get_imports("testns")
  expect_equal(
    imp,
    cbind(
      c("symbol", "findFuncLocals", "untar", "*"),
      c("clisymbols", "codetools", "utils", "foo")
    )
  )
})


test_that("reading a NAMESPACE file, exports", {
  exp <- get_exports("testns", funcs = c("foo", "foobar", "bar"))
  expect_equal(
    exp,
    c("annotate", "print.function_map", "foo", "foobar")
  )
})
