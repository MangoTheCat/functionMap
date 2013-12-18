\name{checkColNames}
\alias{checkColNames}
\title{ Parse alpha value }
\description{
Parses an input alpha value to be used in numeric summary calculations
}                    
\usage{
checkColNames(dNames, cols)
}
\arguments{
\item{dNames}{ Character vector of column names from a data frame }
\item{cols}{ Character vector of column names that should be found in the data frame }
}
\details{
	Checks that all column names specified in "cols" are found in the data frame names (given by "dNames") and produces a neat error message if not
}
\value{
	A TRUE value (if all of "cols" are found in "dNames") or an exception if not
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{

	checkColNames(LETTERS, "D")						# Returns TRUE
	checkColNames(LETTERS, LETTERS[1:5])			# Returns TRUE
	try(checkColNames(LETTERS, c("A", "Hello")))	# Produces an error
}
