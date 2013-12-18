\name{macroEvaluation}
\alias{macroEvaluation}

\title{ Macro Evaluation } 
\description{
  The macro evaluation gives a short summary of all analysis performed for a single 
  replicate of data.
}
\usage{
macroEvaluation(data, macroCode, interimCol = getEctdColName("Interim") , doseCol = getEctdColName("Dose"))
}

\arguments{
  \item{data}{ (Required) Data set to use, typically returned from a micro evaluation step. }
  \item{macroCode}{ (Required) Function used to summarize the micro data.  If the function has arguments "doseCol" and/or "interimCol", the arguments to macroEvaluation are passed to it in addition to the data }
  \item{interimCol}{ (Optional) Name of the interim column, should be a valid name ("INTERIM" by default). See \code{\link{validNames}} }
  \item{doseCol}{ (Optional) Name of the dose column, should be a valid name ("DOSE" by default). See \code{\link{validNames}} } 
}
\details{
  The macro code is first studied to make sure that it is a function that takes a \code{data} argument. 
  The code is then executed against the micro data, and should produce a data frame containing a single 
  row. This is further checked using the \code{\link{checkMacroFormat}} function.
}
\value{
  A data frame that complies with \code{\link{checkMacroFormat}}
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{
  # example of micro data with interim
  microData <- read.csv( 
    system.file( "Runit", "data", "macroEvaluation", "micro0001.csv" , package = "MSToolkit") ) 
  mCode <- function(data) {
    diffMeans <- data$MEAN[ data$DOSE == 100 & data$INTERIM == 0] - 
    data$MEAN[ data$DOSE == 0 & data$INTERIM == 0  ]
    data.frame( SUCCESS = diffMeans > 10, NFINAL = sum(data$N) )
  }
  out <- macroEvaluation( microData, mCode)
  stopifnot( nrow(out) == 1 )
    
}

\keyword{ datagen }

