\name{performAnalysis}
\alias{performAnalysis}

\title{ Runs the analysis code against one dataset } 
\description{
  That function is iteratively called from \code{\link{analyzeRep}} to analyze 
  a replicate dataset at each step defined by the interims.
}
\usage{
performAnalysis(analysisCode, doses, data, software = c("R", "SAS"),  
  includeRows = NULL, interimCol = getEctdColName("Interim"), 
  doseCol = getEctdColName("Dose"), seed = .deriveFromMasterSeed(), 
  workingPath = getwd(), cleanUp = TRUE, tempSasDir = tempdir())
}

\arguments{
  \item{analysisCode}{ (Required) Analysis code: An R function, a reference to an external R script, or a reference to an external SAS script }
  \item{doses}{ (Required) Vector of doses for which estimates are expected }
  \item{data}{ (Required) Input dataset on which to perform analysis }
  \item{software}{ (Optional) Software for analysis: "R" or "SAS" (default is "R") }
  \item{includeRows}{ (Optional) 2 column matrix specifying interims and doses to include in analysis.  By default, no subsets are applied }
  \item{interimCol}{ Interim flag column name }
  \item{doseCol}{ Dose column name }
  \item{seed}{ (Optional) Random number generation seed.  By default, this is derived from the current random seed }
  \item{workingPath}{ (Optional) Working directory for the analysis.  The default is the current working directory }
  \item{cleanUp}{ (Optional) Logical: Should we remove the files that have been passed in and out of SAS? }
  \item{tempSasDir}{ Temporary Sas Directory }
}
\details{
  The function tries to perform the analysis contained in the \code{analysisCode} against the \code{data}.  
  The analysis code specified is either an external file containing code (SAS and R), or an R function (R only).
  
  If software is set to "SAS" the analysis code is assumed to be a reference to an external SAS script.  The SAS code must accept a single dataset called work.infile, and create an output dataset called work.outfile.  The work.outfile dataset must be a valid "Micro Evaluation" structure as specified in the help file for \code{\link{checkMicroFormat}}.
  If the software is "R", the analysis code input must be either an R function or an R script.  The R analysis code must also return a valid "Micro Evaluatoin" structure as specified in function \code{\link{checkMicroFormat}} 
}
\value{
The function should output a valid "Micro Evaluation" data structure as specified in the \code{\link{checkMicroFormat}} help file.
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ \code{\link{checkMicroFormat}}}
\examples{\dontrun{
  
  # Emax example
  anaCode <- function(data){
    with( data, {
      uniDoses <- sort( unique(DOSE))
      outDf <- data.frame( DOSE = uniDoses, 
        MEAN = tapply(RESP, DOSE, mean) , 
        SE   = tapply(RESP, DOSE, sd )  )
      outDf$LOWER <- outDf$MEAN - 2 * outDf$SE
      outDf$UPPER <- outDf$MEAN + 2 * outDf$SE
      outDf$N     <- table(DOSE)[ as.character(uniDoses) ]
      outDf 
    }) 
  }
  
  # example data
  exData <- system.file( "Runit", "data", "analyseRep", "ReplicateData", "replicate0001.csv", package = "MSToolkit")
  out <- performAnalysis(anaCode, data = exData, doses = c(0, 5, 25, 50, 100) )
  checkMicroFormat( out )       # Check the format of the return structure

}}


\keyword{ models }

