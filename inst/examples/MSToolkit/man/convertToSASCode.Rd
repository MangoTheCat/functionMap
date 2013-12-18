\name{convertToSASCode}
\alias{convertToSASCode}

\title{ Convert R to SAS Statements } 
\description{
  Converts R statements to SAS statements, to be included in a batch call to the SAS system
}
\usage{
convertToSASCode(code)
}

\arguments{
  \item{code}{ (Required) R code elements to be converted to SAS statements }
}
\details{
  The function will accept either a character vector or a character matrix with 3 columns.
  
  If the input is a vector, the function assumes these are logical statements to be converted to SAS code.  
  The converted statements are prefixed with an "IF" statement and collapsed based on semi-colons.

  If the input is a 3 column vector, the function will convert the first column of (logical) statments to SAS code, and prefix them with an "IF" statement.
  The 2nd column is pasted together with the first column, seperated by a "THEN" statement.  The 3rd column is pasted together with the third column seperated by an "=" sign. 
  The whole string is collapsed based on ";" symbols and returned.
}

\value{
  A single character string representing the "SAS" version of the input code
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{
\dontrun{

convertToSASCode(c("X > 1", "X < 0", "Y > 0 & Y < 10"))
#  [1] "IF X > 1 ;IF X < 0 ;IF Y > 0 AND Y < 10 ;"
  
sasMat <- cbind(c("X > 1", "X < 0", "Y > 0 & Y < 10"), c("Col1", "Col2", "Col3"), 1:3)
sasMat
#     [,1]             [,2]   [,3]
#[1,] "X > 1"          "Col1" "1" 
#[2,] "X < 0"          "Col2" "2" 
#[3,] "Y > 0 & Y < 10" "Col3" "3" 
convertToSASCode(sasMat)   # Convert matrix of "data change" code
# [1] "IF X > 1 THEN Col1 = 1 ;IF X < 0 THEN Col2 = 2 ;IF Y > 0 AND Y < 10 THEN Col3 = 3 ;"

}}


\keyword{ IO }

