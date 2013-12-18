\name{readData}
\alias{readData}

\title{Read scenario data}
\description{Reads a single element of replicate, macro evaluation or micro evaluation data from the current scenario}
\usage{
readData(dataNumber, dataType = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"), 
	variables = NULL, workingPath = getwd(), method = getEctdDataMethod())
}

\arguments{
  \item{dataNumber}{ (Required) The number of the data entry you wish to read.  This number must be between 0 and 9999 }
  \item{dataType}{ (Optional) The type of data you wish to read - either "ReplicateData", "MacroEvaluation" or "MicroEvaluation".  By default, "ReplicateData" is used }
  \item{variables}{ (Optional) The variables that must be in the data to continue.  No variables are provided by default (so no variable check is performed) }
  \item{workingPath}{ (Optional) The working path of the current scenario.  By default, the current working directory is used }
  \item{method}{ (Optional) Data storage method (i.e. where to extract the data from).  Given by \link{getEctdDataMethod} by default }
}
\value{
	readData will return a data frame containing the desired element of data if it exists.
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{\code{\link{writeData}}}
\examples{
	\dontrun{
   readData(dataNumber = 10, dataType = "Macro")
	 readData(dataNumber = 1, dataType = "Replicate")
	 readData(dataNumber = 800, dataType = "Micro")
 }
}
\keyword{IO}
