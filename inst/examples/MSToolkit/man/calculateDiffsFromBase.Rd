\name{calculateDiffsFromBase}
\alias{calculateDiffsFromBase}
\title{Calculate differences from baseline}
\description{
	Calculate responses differences from baseline for each subject
}
\usage{
calculateDiffsFromBase(data, respCol = getEctdColName("Response"), idCol = getEctdColName("Subject"), 
    timeCol = getEctdColName("Time"), replicateCol = getEctdColName("Replicate"), 
    baseDef = paste(timeCol, "<= 0"), tolerance = 1e-05, removeBaseline = TRUE) 
}
\arguments{
        \item{data}{ Data frame } 
        \item{respCol}{ Response column names (given by \link{getEctdColName} by default)} 
        \item{idCol}{ Subject column names (given by \link{getEctdColName} by default) } 
        \item{timeCol}{ Time column names (given by \link{getEctdColName} by default) } 
        \item{replicateCol}{ Replicate column names (given by \link{getEctdColName} by default) } 
        \item{baseDef}{ Defintion of what "baseline data" is for each subject (default is "Time <= 0") } 
        \item{tolerance}{ Any eventual response values whose absolute } 
        \item{removeBaseline}{ Logical: Should we remove the baseline data once "differences from baseline" have been calculated? }
}
\details{
	The inputs are checked, and the function ensures that \code{respCol}, \code{idCol} and \code{timeCol} are all variables in the dataset (\code{data})
	The \code{baseDef} code is applied to the data in order to create a logical vector highlighting observations classed as being "baseline" values

	If any subjects do not have "baseline" data, a warning is generated and the original data is returned

	For each subject, the baseline response is calculated as the mean response within the baseline period identified.
	Within subject, the baseline response is subtracted from each response value.  If \code{removeBaseline} is TRUE, the baseline data itself is removed from the dataset
}
\value{
	A data frame, where responses have had each subjects baseline response subtracted
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
