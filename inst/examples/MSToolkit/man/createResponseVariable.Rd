\name{createResponseVariable}
\alias{createResponseVariable}
\title{ Create a response from a data set and an equation }
\description{
  Create a response from a data set and an equation
}
\usage{
createResponseVariable(data, equation, preTest = TRUE, subsetSize = 5)
}
\arguments{
  \item{data}{ (Required) The dataset to use, must be a \code{\link{data.frame}} }
  \item{equation}{ (Required) R function that must have a \code{data} argument or character string describing the equation that uses names of the variables in the data.}
  \item{preTest}{ (Optional) Logical Flag. TRUE to try and build the response first with a subset of the data. The subset size is given by the minimum between the number of rows in the data and the \code{subsetSize} argument.}
  \item{subsetSize}{ (Optional) Size of the subset if the \code{preTest} has been requested. }
}
\details{
  Using the \code{preTest} will make the function fail early if the code is wrong. 
  This is typically useful during the first steps of try and error. When the 
  user is confident that the \code{equation} is correct, the \code{preTest} can be set 
  to \code{FALSE}.
}
\value{
  A numeric vector corresponding to the response variable.
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ \code{\link{addResidualError}} adds a residual error to a response. 
  
  The function \code{\link{createResponse} } is the high level function in the 
  response component and acts as a wrapper for \code{createResponseVariable} and 
  \code{\link{addResidualError}}. 
}
\examples{
  # define a data set
  myData <- data.frame( X = c(1,2,1), Y = c(1,1,1), Z = c(1,1,1) )
  
  % added to comply with SF issue 7
  % Tue Jul 24 10:20:20 BST 2007 @430 /Internet Time/
  # function version
  out1 <- createResponseVariable(data = myData, equation = function(data){
    with( data, X + Y + Z)
  })
  
  # same using the character version
  out2 <- createResponseVariable(data = myData, equation = "X+Y+Z")
  stopifnot( identical(out1,out2))
}
\keyword{ datagen }
