\name{createDirectories}
\alias{createDirectories}

\title{Create replicate data, micro evaluation, and macro evaluation directories} 
\description{
This function creates directories for holding data for the current scenario.  These directories
will hold the replicate data, the micro evaluation data and the macro evaluation data. 
}

\usage{
createDirectories(dirNames = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"), workingPath = getwd(), 
	warn = FALSE, method = getEctdDataMethod())
}

\arguments{
  \item{dirNames}{ (Optional) A vector of directories to create under the "workingPath" directory.  By default, directories "ReplicateData", "MicroEvaluation" and "MacroEvaluation" are created }
  \item{workingPath}{ (Optional) The working directory in which to create directories.  The current working directory is used by default }
  \item{warn}{ (Optional) A logical value.  Should warnings from the directory creation be shown?  FALSE by default }
  \item{method}{ Data storage method (given by \link{getEctdDataMethod} by default) }
}

\details{
  Attempts to create directories named in character vector "dirNames" within the "workingPath" directory
  No action is taken if method is "Internal"
}
\value{
  A logical vector the same length as "dirNames", indicating whether or not the corresponding directories were successfully created
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{
  \dontrun{
    # Create 2 of the 3 directories
    createDirectories(dirNames = c("ReplicateData", "MicroEvaluation"))
  }
}
\keyword{data}
