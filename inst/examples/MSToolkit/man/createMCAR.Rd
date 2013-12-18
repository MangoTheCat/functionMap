\name{createMCAR}
\alias{createMCAR}
\title{ Missing Completely at Random }
\description{
  This function adds (or modifies) a "MISSING" flag to a dataset to simulate 
  a missing completely at random behaviour.  
}
\usage{
createMCAR(data, prop = 0, rule, 
  seed = .deriveFromMasterSeed(), flagName = getEctdColName("Missing"))
}
\arguments{
  \item{data}{ (Required) Data frame to which to add missingness }
  \item{prop}{ (Optional) proportion of missingness between 0 and 1.  The default is "0" (so no missingness is generated) }
  \item{rule}{ (Optional) Only observations matching the rule can be flagged as missing.  Be default, all observations are available to be missing }
  \item{seed}{ (Optional) Random seed to use.  Based on the current random seed by default }
  \item{flagName}{ (Optional) name of the missing flag ("MISSING" by default) }
}
\details{
  The missing data is either added to the dataset or modified if it already exist. In the latter
  case, the function only overwrites data that is not already missing. 
}
\value{
  the \code{data} argument to which a MISSING flag is added or modified.
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ \code{\link{createDropout}} for drop out missingness.

\code{\link{parseRangeCode}} to handle the \code{rule} argument.}
\examples{
  
myData <- data.frame( 
  SUBJ   = rep(1:3, each = 3), 
  TIME = rep(0:2, 3)  ) 
createMCAR( myData, prop = 0.1, rule = "TIME > 0")  

\dontrun{
 ## more examples in the unit tests
 file.show( system.file( "Runit", "runit.data.missing.R" , package = "MSToolkit") ) 
}

}
\keyword{ datagen }

