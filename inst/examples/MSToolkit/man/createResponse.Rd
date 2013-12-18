\name{createResponse}
\alias{createResponse}
\title{ High-level function for the response component. }
\description{
  Create a response from a dataset and an equation.
}
\usage{
createResponse(data, equation, name = getEctdColName("Response"), invLink, distribution = "normal", 
  covariance, errStruc = c("Additive", "Proportional", "Log-Normal"), range, digits = 3, 
  seed = .deriveFromMasterSeed(), flagName = getEctdColName("RespOmit"))
}
\arguments{
  \item{data}{ (Required) Data structure to which to add response. }
  \item{equation}{ (Required) R function or character string for creating response. See \code{\link{createResponseVariable}}. }
  \item{name}{ (Optional) Response variable name ("RESP" by default) }
  \item{invLink}{ (Optional) Inverse link function for the predictor. This is an R \code{\link{function}}.  No inverse link function by default}
  \item{distribution}{ (Optional) Outcome variable distribution, one of "n" for normal, "l" for lognormal, "b" for binomial or "p" for poisson.  Default is "Normal" }
  \item{covariance}{ (Optional) Residual Error (CO)VARIANCE, passed to the \code{\link{addResidualError}} function.  Missing by default, resulting in no residual error generated for the reponse }
  \item{errStruc}{ (Optional) Function describing how to apply residual error: "Additive", "Log-Normal" or "Proportional".   "Additive" is the default}
  \item{range}{ (Optional) Range of acceptable values for created response. See \code{\link{parseRangeCode}}.  Missing by default, resulting in no "range" limitation on the response }
  \item{digits}{ (Optional) Number of digits to which to round the response (3 by default)}
  \item{seed}{ (Optional) Random number seed.  By default, this is derived from the current random seed }
  \item{flagName}{ (Optional) "Response Omit" Flag name ("RESPOMIT" by default) }
}
\value{
  A data set containing two columns. 
  \item{RESP}{The actual Response}
  \item{RESPOMIT}{The response OMIT flag. It is named from the value of 
  the \code{flagName} argument.}
}
\note{
In earlier versions of MSToolkit, the "Proportional" Error Structure behaved differently (it assumed parameters passed were on the log scale)
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ The lower-level function of the response component: 
  \code{\link{createResponseVariable}} and \code{\link{addResidualError}} are called 
  by \code{createResponse}.
}
\examples{
  # example data
  myData <- expand.grid( X = 1:2, Y = 1:2, Z = 1:2 )
  
  % added to comply with SF issue 7
  % Tue Jul 24 10:21:14 BST 2007 @431 /Internet Time/
  # function example
  out1 <- createResponse(data = myData, equation   = function(data) with(data, X+Y+Z), 
    covariance = 1, range = "RESP < 3", seed = 96)
  
  # same example using a character representation
  out2 <- createResponse(data = myData, equation   = "X+Y+Z", 
    covariance = 1, range = "RESP < 3", seed = 96)
  stopifnot( identical( out1, out2) )  
}
\keyword{ datagen }
