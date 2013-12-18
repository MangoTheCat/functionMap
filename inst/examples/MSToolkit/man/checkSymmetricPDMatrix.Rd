\name{checkSymmetricPDMatrix}
\alias{checkSymmetricPDMatrix}
\title{ Checks if a matrix is symmetric and positive definite }
\description{
  Checks if a matrix is symmetric and positive definite using
  an eigen value decomposition.
}
\usage{
 checkSymmetricPDMatrix(mat, tol = 1e-06)
}
\arguments{
  \item{mat}{ (Required) The matrix to check }
  \item{tol}{ (Optional) The tolorence to use when comparing the value to 0.  The default is "1e-06" }
}
\value{
  The function does not return anything. It only generate an error 
  if something goes wrong.
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{\dontrun{
  checkSymmetricPDMatrix( diag(4), 4)
  \dontrun{
    checkSymmetricPDMatrix( -diag(4),4 )
  }
}}
\keyword{ error }
