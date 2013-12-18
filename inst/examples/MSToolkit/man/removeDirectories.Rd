\name{removeDirectories}
\alias{removeDirectories}

\title{Remove directories} 
\description{
	This function removes directories containing data for the current scenario.
}
\usage{
removeDirectories(dirNames = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"), 
	workingPath = getwd(), method = getEctdDataMethod())
}

\arguments{
  \item{dirNames}{ (Optional) A vector of directories to remove from the "workingPath" directory.  Default is to remove "ReplicateData", "MicroEvaluation" and "MacroEvaluation" directories }
  \item{workingPath}{ (Optional) The working directory from which to remove the directories.  By default, directories are removed from the current working directory }
  \item{method}{ Data storage method (given by \link{getEctdDataMethod} by default) }
}

\details{
  dirNames must be a character vector.  The function will attempt to remove each of the directories identified in the dirNames vector from within the "workingPath" directory
  If method is "Internal", the DataStore is emptied and no directories are modified
}  
\value{
  A logical vector the same length as the dirNames, indicating whether or not the corresponding directories were successfully removed
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{
  \dontrun{
    createDirectories(dirNames = c("ReplicateData", "MicroEvaluation"))
    removeDirectories(dirNames = c("ReplicateData", "MicroEvaluation"))
  }
}
\keyword{data}
