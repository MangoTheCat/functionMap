\name{writeData}
\alias{writeData}
\title{Write scenario data}
\description{
Writes a single element of scenario data (replicate, micro evaluation or macro evaluation) to a file.
}
\usage{
writeData(dat, dataNumber, dataType = c("ReplicateData", "MacroEvaluation", "MicroEvaluation"), 
 workingPath = getwd(), append = FALSE, prefix = switch(dataType, ReplicateData = "replicate", MicroEvaluation = "micro", MacroEvaluation = "macro"), method = getEctdDataMethod())
}

\arguments{
  \item{dat}{ (Required) A data frame containing the data to be written }
  \item{dataNumber}{ (Required) The entry number of the data - this must be a whole number between 1 and 9999 }
  \item{dataType}{ (Optional) The type of data to be written - this must be either "MacroEvaluation" "MicroEvaluation" or "ReplicateData" (default is "ReplicateData") }
  \item{workingPath}{ (Optional) The path of the current scenario.  The working directory is used by default }
  \item{append}{ (Optional) A logical value.  If the file for the entry of data you are specifying already exists, setting append to TRUE will cause writeData to append to the end of that file.  Otherwise it will overwrite the file.  The default is FALSE }
  \item{prefix}{ (Optional) Prefix for exported file name }
  \item{method}{ (Optional) Data storage method (i.e. where to extract the data from).  Given by \link{getEctdDataMethod} by default }
}
\value{
  Returns TRUE if the file was successfully created
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{\code{\link{readData}}}
\examples{
  \dontrun{
    createDirectories()
    x <- matrix(data = c(0,0,0,0,0,4,4,4,4,4,0,10,25,50,100,0.76,16.91,33.12,49.39,65.73,0.21,1.3,2.44,2.33,4.55,
    -6.96,9.19,25.4,41.67,58,8.48,24.63,40.84,57.12,73.45,50,50,50,50,50,0,0,0,0,0,0,0,0,0,0), nrow = 5, ncol = 10)
    x <- as.data.frame(x)
    colnames(x) <- c("INTERIM","INTERIMC","D","MEAN","SE","LOWER","UPPER","N","DROPPED","STOPPED")
    writeData(dat = x, dataNumber = 199, dataType = "Micro")
    
    x <- as.data.frame(matrix(c(0,1),ncol=2,nrow=1))
    colnames(x) <- c("Dropped", "Stopped")
    writeData(dat = x, dataNumber = 5, dataType = "Macro")
  }
}
\keyword{IO}

