\name{parseCovMatrix}
\alias{parseCovMatrix}

\title{Parse a covariance matrix} 

\description{
  Creates a symmetric positive definite matrix from a vector if possible, or checks if a given matrix is symmetric positive definite.
}

\usage{
  parseCovMatrix(values, nCov, tol = 1e-06)
}

\arguments{
  \item{values}{ (Required) Either a matrix or a mixed character-numeric vector }
  \item{nCov}{ (Required) The number of rows and columns that should be available in the resulting matrix }
  \item{tol}{ (Optional) Numerical tolerance for the positive-definiteness check.  By default, the tolerance is used as 1e-06 }
}
\details{
parseCovMatrix attempts to create a symmetric positive definite matrix of dimension nCov x nCov.  If values is a matrix,
 parseCovMatrix will simply check that it is symmetric and positive definite up to a tolerance as inidicated by tol.
 If it is a numeric vector, parseCovMatrix will try to create an nCov x nCov positive definite symmetric matrix, with the method
 of creation depending on values and nCov.  If values contains a single entry, an nCov x nCov diagonal matrix with that single
 value repeated nCov times will be created.  If the number of entries in "values" is equal to "nCov", parseCovMatrix will create
 a diagonal matrix whose diagonal will be equal to "values".  if the number of entries in "values" is equal to nCov * (nCov+1) / 2,
 parseCovMatrix will create a positive definite symmetric matrix with the entries for the lower triangle taken from "values".  If 
 none of these conditions hold or if the entries of "values" are not compatible with positive definite symmetric matrices, an
 error will be printed.  
}
\value{A positive definite symmetric matrix}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }

\seealso{\code{\link{checkSymmetricPDMatrix}}  }
\examples{
  parseCovMatrix(2, nCov = 3)
  parseCovMatrix(c(1,2,3), nCov = 3)
  parseCovMatrix(c(1,2,4), nCov = 2)
}
\keyword{datagen}

