\name{parseCharInput}
\alias{parseCharInput}

\title{Parse a comma seperated string} 
\description{
  The "parseCharInput" function converts a comma-seperated string into a vector by splitting the string along the commas.  If "convertToNumeric" is true, the elements of the string will be converted into numbers and a numeric vector will be returned.
}
\usage{
parseCharInput(input, convertToNumeric = TRUE, sort = FALSE, 
  expected, msg = "wrong length", checkdup = FALSE, 
  missingMsg = "Unknown", checkProb = FALSE, valid = FALSE)
}
\arguments{
  \item{input}{ (Required) The string to be processed }
  \item{convertToNumeric}{ (Optional) A logical value.  Should the input should be converted into numerics?  TRUE by default }
  \item{sort}{ (Optional) A logical value.  Should the resulting vector be sorted.  FALSE by default }
  \item{expected}{ (Optional) The expected length of the output vector.  No check is performed by default }
  \item{msg}{ (Optional) Error message to print if the input is not of the correct length specified by "expected".  Default is "Wrong length" }
  \item{checkdup}{ (Optional) A logical value. Should an error be generated if there are any duplicated value?  FALSE by default }
  \item{missingMsg}{ (Optional) Error message to print if the input is missing.  Default is "Unknown" }
  \item{checkProb}{ (Optional) A logical value.  Should the function check that the output vector sums to one?  FALSE by default }
  \item{valid}{ (Optional) A logical value. Should the \code{\link{validNames}} function be used to check the validity of the extracted names?  FALSE by default }
}
\value{
  A numeric vector or a character vector depending on the \code{convertToNumeric} flag.
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ \code{\link{parseHashString}} uses \code{parseCharInput} sequentially. }
\examples{
  parseCharInput( "1,2,4")
  parseCharInput( "1,6,4", sort = TRUE )
 
  \dontrun{
  ## see also the unit tests for more examples
  file.show( system.file( "Runit", "runit.supportfunctions.R" , package = "MSToolkit"))  
  }
}
\keyword{misc}
