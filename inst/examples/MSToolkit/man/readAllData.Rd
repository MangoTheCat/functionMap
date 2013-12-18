\name{readAllData}
\alias{readAllData}

\title{Read scenario data}
\description{Reads a single element of replicate, macro evaluation or micro evaluation data from the current scenario}
\usage{
readAllData(dataType = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"), 
    workingPath = getwd(), dirName = dataType, 
    prefix = switch(dataType, ReplicateData = "replicate", MicroEvaluation = "micro", MacroEvaluation = "macro"), 
    replicates = NULL, consolidate = TRUE, replicateCol = getEctdColName("Replicate"), method = getEctdDataMethod()) 
}

\arguments{
  \item{dataType}{ (Optional) The type of data you wish to read - either "ReplicateData", "MacroEvaluation" or "MicroEvaluation".  Default is "ReplicateData" }
  \item{workingPath}{ (Optional) The working path of the current scenario.  The current working directory is used by default. }
  \item{dirName}{ (Optional) Subdirectory of workingPath from which to read data.  By default, this is taken as the same as the "dataType" input }
  \item{prefix}{ (Optional) Prefix of file name to use (eg. "micro" for Micro Evaluation files).  Default is "Replicate" when dataType is "ReplicateData", "Micro" when dataType is "MicroEvaluation" or "Macro" when dataType is "MacroEvaluation" }
  \item{replicates}{ (Optional) A vector identifying a subset of replicates to import from the directory.  By default, all replicates are imported }
  \item{consolidate}{ (Optional) Should the function combine the seperate data frames into a single data frame.  TRUE by default }
  \item{replicateCol}{ (Optional) Name for created "Replicate" variable in the consolidated data frame (default given by \link{getEctdColName}) }
  \item{method}{ (Optional) Data storage method (i.e. where to extract the data from).  Given by \link{getEctdDataMethod} by default }
}
\value{
	If consolidate is FALSE, the function will return a list of data frames.  
	If consolidate is TRUE, the function will combine this list into a single data frame for return 
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{\code{\link{writeData}}, \code{\link{readData}}}
\examples{
	\dontrun{
   readAllData(dataType = "Macro", consolidate = FALSE)
	 readAllData(dataType = "Micro")	 
 }
}
\keyword{IO}
