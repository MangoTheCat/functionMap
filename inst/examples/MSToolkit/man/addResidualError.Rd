\name{addResidualError}
\alias{addResidualError}
\title{ Add residual error to the response. }
\description{
  Adds residual error to a generated response, based on a supplied variance-covariance matrix
}
\usage{
addResidualError(response, covariance, errStruc = c("Additive", "Proportional", "Log-Normal"), seed = .deriveFromMasterSeed())
}

\arguments{
  \item{response}{ (Required) Numeric vector of response data }
  \item{covariance}{ (Required) Residual error (co)variance.  This would be the lower-triangle of the matrix, or the matrix itself. The function \code{\link{parseCovMatrix}} is used to ensure that the matrix is in the right format. }
  \item{errStruc}{ (Optional) Function describing how to apply residual error: "Additive", "Log-Normal" or "Proportional".   "Additive" is the default}
  \item{seed}{ (Optional) Random seed to use.  Derived from the current random seed by default }
}
\details{
  The first step in the algorithm will be to set the random number seed to 
  the "seed" argument.  
  Then, a number of samples will be drawn from a multivariate normal 
  distribution with mean 0 and covariance set by the (parsed) \code{covariance} input. 
  The number of samples to take will be set by the number of elements in the 
  "response" vector.

  Based on the "errStruc" input, the algorithm should continue as follows. 
 * If it is Additive, the residual errors should be added to the response vector. 
 * If it is Log Normal, the response vector is multiplied by the exponentiated residual error.
 * If it is Proportional, the response vector is multiplied by "1 + the residual error".
}
\value{
  A numeric vector
}
\note{
In earlier versions of MSToolkit, the "Proportional" Error Structure behaved differently (it assumed parameters passed were on the log scale)
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ 
  This function is typically not directly called by the user, but rather called 
  by the function \code{\link{createResponse}} that is the high-level function 
  for the response component. 
  
  The function \code{\link{createResponseVariable}} is also in the response component. 
  It create the the response onto which \code{addResidualError} adds error.
}
\examples{
  myVec <- 1:10
  addResidualError(response = myVec, covariance = "1" )
}
\keyword{ datagen }

