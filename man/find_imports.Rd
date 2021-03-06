% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/namespaces.R
\name{find_imports}
\alias{find_imports}
\title{Check if a list of function names were imported from a package}
\usage{
find_imports(path, functions)
}
\arguments{
\item{path}{The folder containing the (uncompressed) R package.}

\item{functions}{The functions to find, a character vector.}
}
\value{
A character vector, the names of the environments
  for the \code{functions}.
}
\description{
Check if a list of function names were imported from a package
}
\keyword{internal}

