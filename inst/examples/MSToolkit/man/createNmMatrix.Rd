\name{createNmMatrix}
\alias{createNmMatrix}
\title{Creates a matrix based on a "NONMEM style" vector}
\description{
Creates a matrix based on a vector specifying the lower diagonal elements (as would be defined in NONMEM outputs)
}
\usage{
createNmMatrix(x, dimnames = NULL, byrow = TRUE)
}
\arguments{
        \item{x}{ Vector of values to be placed in the matrix } 
        \item{dimnames}{ Optional dimension names for output matrix }
        \item{byrow}{ (Optional) Should we read in the data into the matrix diagonal by row (default) or by column }
}
\details{
	Places the values in the lower diagonal of an output matrix, and reflects these values across the diagonal.
	Adds dimension names if provided
}
\value{
	A matrix
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{
	createNmMatrix(1:3)
	createNmMatrix(1:6)
	createNmMatrix(1:10, list(LETTERS[1:4], letters[1:4]))
}
\keyword{ matrix }
\keyword{ nonmem }
