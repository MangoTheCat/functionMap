\name{validNames}
\alias{validNames}

\title{ Check if a character string is a valid R variable name } 
\description{
  Checks if a character string is a valid R variable name using regular expressions.
}

\usage{
  validNames(...)
}

\arguments{
  \item{\dots}{ (Required) A collection of character vectors to be checked }
}
\details{
  A valid R name starts with a letter or a dot followed by a non-numerical character and contains only letters, numbers and dots.
}

\value{
  None. The function is simply generates an error if an invalid name is passed to it.
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} } 
\examples{
  # correct name
  validNames("DOSE")
  \dontrun{
    # wrong name
    validNames(".334fsedqw")
  }
}

\keyword{error}
