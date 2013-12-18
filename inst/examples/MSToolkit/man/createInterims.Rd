\name{createInterims}
\alias{createInterims}
\title{ create the interim flag }
\description{
  That function creates the \emph{INTERIM} flag that identifies 
  which subject is included in the analysis at every interim stage. 
}
\usage{
createInterims(subjects, proportion, seed = .deriveFromMasterSeed(), 
  idCol = getEctdColName("Subject"), interimCol = getEctdColName("Interim"), method = "Sample")
}
\arguments{
  \item{subjects}{ (Required) Vector of subjects or number of subjects }
  \item{proportion}{ (Required) Vector of proportions of subjects in each interim.  This is a vector of cumulative proportions }
  \item{seed}{ (Optional) The random number generation seed to use.  By default, this is derived from the current random seed }
  \item{idCol}{ (Optional) Name of the subject column ("SUBJ" by default) }
  \item{interimCol}{ (Optional) Name of the interim column ("INTERIM" by default) }
  \item{method}{ (Optional) Method to use. Must be one of \emph{Sample} or \emph{Proportion}.  Default is \emph{Sample} }
}
\details{
  If the "method" argument is set to "Sample", the interim numbers should be assigned using the following algorithm:
Store the number of subjects (nSubjects) and the number of interims (nInterims)
Append a zero to the start of the proportions and take differences to get a vector of "non-cumulative" probabilities
Take "nSubjects" samples from the values "1" to "nInterims" with probabilities set to the derived "non-cumulative" probabilities
Replace the "interim" variable with these values.

If the "method" argument is set to "Proportion", the interim numbers should be assigned using the following algorithm:
Store the number of subjects (nSubjects) and the number of interims (nInterims)
Convert the cumulative proportions to "non-cumulative" probabilities
Allocate a "base" number of subjects to each interim (found by multiplying nSubjects by the set of probabilities, then take the floor of the resulting values)
If any subjects are left at this stage, use the "sample" method (below) to allocate the remaining subjects to interims
"Shuffle" the interim variable so that the results are not ordered.
}
\value{
  A data frame containing the following variables:
  \item{SUBJ}{Subject identifier, named after \code{idCol}.}
  \item{INTERIM }{Interim flag, named after \code{interimCol}.}
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ \code{\link{generateData}} }
\examples{
  # first interim with 10% of the subjects
  # second with 30%
  # third with 60%
  createInterims(100, proportion  = ".1,.3,.6" )
  
  \dontrun{
    ## more examples in the unit tests
    file.show( system.file("Runit", "runit.data.interim.R",  package = "MSToolkit") )
  }
  
}
\keyword{ datagen }

