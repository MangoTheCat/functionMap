
f <- function(foo, bar) {
  f()
  g()
}

g <- function(foobar) {
  h()
  utils::untar(foobar)
}

h <- function() {
  print('hello')
}

iso <- function() {
  ## This is never called
}

k <- function() {
  x()
}
