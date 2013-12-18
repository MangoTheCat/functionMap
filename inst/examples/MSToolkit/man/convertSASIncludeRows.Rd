\name{convertSASIncludeRows}
\alias{convertSASIncludeRows}
\title{Create a SAS 'IF' statement}
\description{
	Create a SAS 'IF' statement, used to describe rows to include in a SAS analysis
}
\usage{
convertSASIncludeRows(includeRows, doseCol = getEctdColName("Dose"), interimCol = getEctdColName("Interim"))
}

\arguments{
  \item{includeRows}{ Row inclusion matrix (2 columns: Interim value and Dose value) }
  \item{doseCol}{ Dose column name, given by \link{getEctdColName} by default }
  \item{interimCol}{ Interim column name, given by \link{getEctdColName} by default }
}
\details{
  Takes a matrix of interim and dose values, and produces a "SAS style" IF statement
}
\value{
A single character string
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{
convertSASIncludeRows( cbind(rep(1:2, each = 3), c(0, 15, 30, 0, 30, 45) ) )
} 
\keyword{ SAS }
