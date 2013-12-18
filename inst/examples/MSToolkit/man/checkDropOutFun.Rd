\name{checkDropOutFun}
\alias{checkDropOutFun}

\title{ check the user supplied drop out function } 
\description{
  This function performs checks to ensire that the user-supplied dropout 
  function is correct.  
}
\usage{
checkDropOutFun(fun, data, sizeSubset = 5, useSubset= TRUE, ...)
}

\arguments{
  \item{fun}{ (Required) Function to check for validity }
  \item{data}{ (Required) Dataset to use for performing the valildity check }
  \item{sizeSubset}{ (Optional) Number of "initial" rows from the provided data to use in the test for the validity of the function.  The default is "5" }
  \item{useSubset}{ (Optional) Should the check operate on a subset of the data, as opposed to the entire dataset?  TRUE by default }
  \item{\dots}{ (Optional) Extra arguments to be passed directly to the function being tested.  No extra arguments are passed by default }
}
\details{
  A correct dropout function must have at least a \code{data} argument, and
  it must return a numeric vector containing the values 0 and 1 having as its length 
  the number of rows of the dataset it is given. 
}
\value{
  Nothing. Function only used for the sied effect of generating an error 
  if the function is not correct.
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\note{  
  if \code{useSubset} is set to TRUE, only a subset of the data 
  is used to perform the check. The size of the subset is then 
  the minimum of the \code{sizeSubset} and the number of rows in the dataset.
}
\seealso{ 
  \code{\link{createDropout}} calls this function before creating the drop out flag.
}

\examples{
  dFun <- function(data, prop) sample(0:1, nrow(data), TRUE, c(1-prop, prop))
   testData <- data.frame(
     SUBJ=rep(1:10, each=5), 
     TIME=rep(0:4, 10), 
     VALUE=rnorm(50))
  checkDropOutFun( dFun, testData, prop = .2 )
  
  \dontrun{
    # wrong function
    checkDropOutFun( max, testData )
    
    # function that does not exist
    checkDropOutFun( "XXXX", testData )
    
    # function that does not exist
    checkDropOutFun( XXXX, testData )
    
  }

}
\keyword{ error }
