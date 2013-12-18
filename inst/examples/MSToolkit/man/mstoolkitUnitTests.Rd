\name{mstoolkitUnitTests}
\alias{mstoolkitUnitTests}
\title{ Unit/System Tests suite for the MSToolkit package }
\description{
  This function enables the user of the MSToolkit package to run the 
  unit tests of the package.
}
\usage{
mstoolkitUnitTests(htmlreport = FALSE, showdetails = TRUE)
}
\arguments{
  \item{htmlreport}{ (Optional) A logical value.  Print an html report for unit and system tests?  FALSE by default }
  \item{showdetails}{ (Optional) A logical value.  Print the details in the text summary of the unit tests?  TRUE by default }
  
}
\details{
  The unit tests are based on the RUnit framework.
}
\value{
  The function does not return anything. It writes two files: \code{reportUT.html} for the unit tests
 and \code{reportST.html} for the system tests.
}
\references{ 
  Matthias Burger, Klaus Juenemann and Thomas Koenig (2007). 
  RUnit: R Unit test framework. R package version 0.4.17.
  https://sourceforge.net/projects/runit/

  RUnit and R CMD check 
  http://wiki.r-project.org/rwiki/doku.php?id=developers:runit
  
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ \code{\link[RUnit]{runTestSuite}} }
\examples{
  \dontrun{
    mstoolkitUnitTests( htmlhelp = TRUE )
    
    # unit tests
    browseURL("reportUT.html")
    
    # system tests
    browseURL("reportST.html")
    
  }
}
\keyword{ error }

