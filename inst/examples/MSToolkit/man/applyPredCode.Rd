\name{applyPredCode}
\alias{applyPredCode}
\alias{parsePredCode}
\title{ Parses and applys NONMEM statements } 
\description{
	Parses NONMEM statements and attempts to apply them to a data frame in order to create a response variable
}
\usage{

applyPredCode(df, pred, respCol = getEctdColName("Response"), report = TRUE, 
    keepCols = respCol, verbose = getEctdVerbose()) 

parsePredCode(model, respCol = getEctdColName("Response")) 

}

\arguments{
  \item{df}{ (Required) Data frame to which parsed NONMEM statements are to be applied }
  \item{pred}{ Character vector of NONMEM statements }
  \item{respCol}{ Response column name ( iven by \link{getEctdColName} by default) }
  \item{report}{ Logical: should a textual report be produced? }
  \item{keepCols}{ Character vector of column names to retain in the return dataset }
  \item{verbose}{ Logical: Should verbose logging be used? ( given by \link{getEctdVerbose} by default) }
  \item{model}{ File containing analysis code (for R or SAS) or an R function for analysis (R only) }
}
\details{
  The \code{applyPredCode} function calls the \code{parsePredCode} function in order to convert the NONMEM statements to executable R statements.
  The \code{applyPredCode} function then iteratively "tries" to apply these statements to the data ("df"), producing a textual report of the process if "report" is set to TRUE.
  The "keepCols" columns from the updated dataset are then returned 
}
\value{
  A dataset with new columns added
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{

parsePredCode(c(
	"X = 1",
	"IF (X.EQ.1.OR.Y.GT.0) STUD = 1",
 	"NEWVAR = THETA(1) + EXP(ETA(2))**LOG(EPS(1))"
))

}

\keyword{ NONMEM }
