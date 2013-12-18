\name{checkMacroFormat}
\alias{checkMacroFormat}
\title{ check the macro format dataset }
\description{
  This function is used to perform checks on the structure of the macro format dataset
  produced by the user-supplied macro code in \code{\link{analyzeData}}.
}
\usage{
checkMacroFormat(data)
}
\arguments{
  \item{data}{ (Required) Dataset to be checked to ensure it is a valid "Macro Evaluation" sructure}
}
\details{
  The \code{\link{checkMacroFormat}} function checks that the data passed to it is a valid "Macro Evaluation" structure.
  A valid "Macro Evaluation" structure is an R data frame containing only 1 row of data.
}
\value{
  Nothing. Only used for error generation side effects.
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ 
  \code{\link{analyzeData}}
}
\examples{
  checkMacroFormat(iris[1,])
  
  \dontrun{
    # not a data frame
    checkMacroFormat( 1:10 )
    
    # more than one row
    checkMacroFormat(iris[1:2,])
  
  }
}
\keyword{ IO }

