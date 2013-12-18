\name{interimAnalysis}
\alias{interimAnalysis}

\title{ interim Analysis } 
\description{
  This function runs the interimCode to takes decisions on dose to drop for 
  next interim step, the decision take can also potentially be to stop 
  the analysis at this interim stage.
}
\usage{
interimAnalysis(data, interimCode, uniqueDoses = NULL)
}

\arguments{
  \item{data}{ (Required) Dataset to analyze  }
  \item{interimCode}{ (Required) Function to execute against the data that should produce a list structure as described below}
  \item{uniqueDoses}{ Unique study doses}
}
\value{
  The output of the interim code should be an R "list" structure.  

  If an empty list is returned, this signifies that there are no instructions to apply following this interim analysis (ie. don't stop the trial and do not drop any doses)

  If the return list contains a "STOP" element, and the "STOP" element is a logical value of length 1, this will be used to determine whether the trial should be stopped at this interim.
  
  If the return list contains a "DROP" element, and the "DROP" element is a vector of dose values, this will be used to determine which doses to drop before the next interim.
  
  Any list elements other than "DROP" or "STOP" will be ignored.
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }

\examples{
  myData <- data.frame(DOSE=c(0, 15, 30), TEST = 1:3)
  myFun <- function(data) {
    outList <- list()
    outList$STOP <- any(data$TEST) > 5
    myTest <- data$TEST > 2
    if (any(myTest)) outList$DROP <- data$DOSE[myTest]
    outList
  }
  interimAnalysis(myData, myFun )
}


\keyword{ datagen }

