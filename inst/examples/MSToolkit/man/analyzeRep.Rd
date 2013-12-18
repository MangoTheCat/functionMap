\name{analyzeRep}
\alias{analyzeRep}

\title{ Analyse a single replicate of data } 
\description{
  The \code{analyzeRep} function analyzes a single replicate of data, possibly at different interim levels.
}
\usage{
  analyzeRep(analysisCode, replicate, removeMissing = TRUE, removeParOmit = TRUE, 
  removeRespOmit = TRUE, interimCode, software = c("R", "SAS"), seed = .deriveFromMasterSeed(), 
  parOmitFlag = getEctdColName("ParOmit"), respOmitFlag = getEctdColName("RespOmit"), missingFlag = getEctdColName("Missing"), interimCol = getEctdColName("Interim"), doseCol = getEctdColName("Dose"), 
  initialDoses = NULL, stayDropped = TRUE, fullAnalysis = TRUE, workingPath = getwd(), method = getEctdDataMethod())
}

\arguments{
  \item{analysisCode}{ (Required) File containing analysis code (for R or SAS) or an R function for analysis (R only) }
  \item{replicate}{ (Required) Replicate number of data to be analyzed }
  \item{removeMissing}{ (Optional) Should rows marked as 'Missing' during the data generation step be removed from the data before analysis is performed?  TRUE by default }
  \item{removeParOmit}{ (Optional) Should any rows marked as 'Omitted' during the parameter data generation step (ie. parameters out of range) be removed from the data before analysis is performed?  TRUE by default }
  \item{removeRespOmit}{ (Optional) Should any rows marked as 'Omitted' during the response generation step (ie. responses out of range) be removed from the data before analysis is performed?  TRUE by default }
  \item{interimCode}{ (Optional) An R function to be applied to interim datasets in order to creation interim decisions.  See the help file for the \code{\link{interimAnalysis}} function for more information.  By default, no functions is provided, resulting in no interim analyses being performed }
  \item{software}{ (Optional) The software to be used for analysis: either "R" or "SAS".  "R" is the default software used }
  \item{seed}{ (Optional) Random number seed to use for the analysis.  Based on the current random seed by default }
  \item{parOmitFlag}{ (Optional) Parameter omit flag name.  "PAROMIT" by default }
  \item{respOmitFlag}{ (Optional) Response omit flag name.  "RESPOMIT" by default }
  \item{missingFlag}{ (Optional) Missing flag name.  "MISSING" by default }
  \item{interimCol}{ (Optional) Interim variable name.  "INTERIM" by default }
  \item{doseCol}{ (Optional) Dose variable name.  "DOSE" by default }
  \item{initialDoses}{ (Optional) For interim analyses, which doses should be present in interim 1?  All are included by default }
  \item{stayDropped}{ (Optional) For interim analyses, if a dose is dropped, should it stay dropped in following interims (as opposed to allowing the interim step to reopen the dose) }
  \item{fullAnalysis}{ (Optional) Should a "full" analysis be performed on all doses?  Default TRUE }
  \item{workingPath}{ (Optional) Root directory in which replicate data is stored, and in which we should perform the analysis.  Current working directory is used by default }
  \item{method}{ Data storage method (ie. where the replicate data is stored).  Given by \link{getEctdDataMethod} by default }
}
\details{
  The \code{\link{analyzeRep}} function calls the \code{\link{performAnalysis}} function in order to analyze and summarize a single simulated replicate dataset (held in the "ReplicateData" subdirectory of the specified working path).

  The first step of the analysis is to use the removeMissing, removeParOmit and removeRespOmit flags (together with the missingFlag, parOmitFlag and respOmitFlag inputs) in order to subset the data if required.  For example, we may wish to remove all observations flagged as "missing" in an earlier simulation of subject dropout.  The subset is applied to the data before the analysis.

  The analysis code must be either an R function, a reference to an external R script, or a reference to an external SAS script.  If the software is set as "SAS", it is assumed that the analysisCode is an external SAS script.
  If the analysis code is a SAS script, it must accept a single dataset called work.infile, and create an output dataset called work.outfile.  The work.outfile dataset must be a valid "Micro Evaluation" structure as specified in the help file for \code{\link{checkMicroFormat}}.    
  If the software is "R", the analysis code input must be either an R function or an R script.  The R analysis code must also return a valid "Micro Evaluatoin" structure as specified in function \code{\link{checkMicroFormat}} 

  The first step in \code{\link{analyzeRep}} is to perform a full analysis on the data (which has possibly been subset be the remove* inputs).  Following the analysis, the \code{\link{checkMicroFormat}} function is used to ensure the return data is a valid "Micro Evaluation" data structure.
  The return structure is appended with drop and stop flags (set to 0) and with interim variables (where interim is "FULL").
  
  If the interimCode has been specified, and the "interimCol" variable is found in the data, interim analyses will be performed iteratively on sections of the data.  The interimCode input must be an R function that returns a suitable list structure as described in the \code{\link{interimAnalysis}} help file.
  For each value of "interimCol", the analysis will be performed on a section of data (using a call to \code{\link{performAnalysis}}), and the return from the analysis will be checked (using a call to \code{\link{checkMicroFormat}}).  The "Micro Evaluation" output is then passed to the \code{\link{interimAnalysis}} function and the return list checked for instruction.
  If any return interim list includes doses to "DROP", the doses will be removed from future analyses.  If the "STOP" flag in the list is set to "TRUE", the analysis is stopped at this interim.
  
  Finally, all micro evaluation outputs (with appended interim variables and drop/stop flags) are combined and returned.  
}
\value{
  A "Micro Evaluation" structure with additional variables (interim column, drop flag and stop flag)
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ 
  \code{\link{performAnalysis}} is called by \code{analyzeRep} to perform
  each analysis on the subset of data. \code{\link{interimAnalysis}} executes the 
  \code{interimCode} and updates the data changes accordibgly.
  
  \code{\link{analyzeData}} calls \code{analyzeRep} sequentially.
}
\examples{
\dontrun{  
  # Analysis Code
  emaxFun <- function(data){
    library(DoseResponse)
    with( data, {
      uniDoses <- sort( unique(D))                                                                    
      eFit <- emaxalt( RESP, D )
      outDf <- data.frame( D = uniDoses, 
        MEAN = eFit$dm[as.character(uniDoses)], 
        SE = eFit$dsd[as.character(uniDoses)] )
      outDf$LOWER <- outDf$MEAN - 2 * outDf$SE
      outDf$UPPER <- outDf$MEAN + 2 * outDf$SE
      outDf$N     <- table(DOSE)[ as.character(uniDoses) ]
      outDf 
     }) 
   }                                                                                                                   
  
  analyzeRep(replicate = 1, analysisCode = emaxFun)
}
}

\keyword{ datagen }
\keyword{ IO }
\keyword{ models }


