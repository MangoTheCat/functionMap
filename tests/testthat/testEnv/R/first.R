
## This file is supposed to be parsed first

f <- function(foo = TRUE) if (foo) f(FALSE) else NULL
