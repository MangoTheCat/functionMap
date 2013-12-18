\name{createEmptyMicro}
\alias{createEmptyMicro}
\title{Create an empty micro evaluation data frame}
\description{
	Creates an "empty" (i.e. filled in with NA values) micro evaluation data frame that has its column names 
  and dose column name given by arguments.  
}
\usage{
createEmptyMicro(doses, doseCol = "DOSE", microColumnNames = c("doseName" = doseCol, "MEAN", "SE", "LOWER", "UPPER", "N"))
}

\arguments{
  \item{doses}{ (Required) A mixed numeric or character vector of dose data that will be the dose column of the data frame.  It must not contain any duplicate entries. }
  \item{doseCol}{ (Optional) A string indicating the name of the column which will hold the "doses" vector.  "DOSE" by default }
  \item{microColumnNames}{ (Optional) A character vector containing the names of the columns of the micro evaluation data frame.  By default, variables "MEAN", "SE", "LOWER", "UPPER" and "N" are created in addition to the dose variable }
}
\details{
	createEmptyMicro will return a data frame whose column names are given by \code{microColumnNames}. The first column of the resulting data frame will
  contain the \code{doses} vector by default.  The data frame will have as many rows as there are entries in \code{doses}.  All of the
  frame's other entries will be NA.	This function will generate an error if "doses" contains duplicate entries or if the dose
  column name specified by \code{doseCol} doeses not occur in \code{microColumnNames}. 
}
\value{
	createEmptyMicro returns a data frame that is as described above.
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\note{ Future versions of MSToolkit will relax the format of the MicroEvaluation dataset allowing a more free structure for this dataset. }
\seealso{\code{\link{checkMicroFormat}}  }
\examples{
  createEmptyMicro(doses = c(10, 20 ,30, 40))
  createEmptyMicro(doses = c(5, 25), doseCol = "D", microColumnNames = c("D", "MEAN", "SE", "LOWER"))
}
\keyword{data}
