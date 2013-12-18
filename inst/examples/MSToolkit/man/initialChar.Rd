\name{initialChar}
\alias{initialChar}

\title{Convert the first letter that occurs in a string to lower case} 
\description{By default, initialChar finds the first letter that occurs in a string and returns its lower case version.  The 
user may use the "adm" argument to restrict the admissable letters which may be returned (by default this will be all possible
lower case letters).
}
\usage{
initialChar(txt, adm = "[:lower:]", err = "Not acceptable value")
}

\arguments{
  \item{txt}{ (Required) The string input to process }
  \item{adm}{ (Optional) Regular expression which may be used to restrict the set of letters which will be returned.  By default it will allow all lower case letters }
  \item{err}{ (Optional) The error message which will be printed if no admissable letters (as determined by adm) are found.  By default, message "Not acceptable value" will be displayed } 
}
\value{
initalChar returns a lower case version of the first letter contained in the string "txt" if that character is admissable.  
If it is not (or no letter is found), an error message will be printed.
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\note{  
This function is not case sensitive. 
}

\examples{
initialChar("100 Kilometers")
\dontrun{
   # The first character is "A", but "a" is not in the list of admissable letters 
    initialChar("Allen, Alistair, Atticus", adm = "irt" )
  }
}

\keyword{misc}
