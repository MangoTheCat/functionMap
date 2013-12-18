\name{createDropout}
\alias{createDropout}

\title{ Adds dropout to a simulated dataset } 
\description{
  Applies a given dropout function to a simulated trial dataset, in order to create a "missing" variable representing subject dropout
}
\usage{
createDropout(data, dropFunc, 
  seed = .deriveFromMasterSeed(), idCol = getEctdColName("Subject"),
  timeCol = getEctdColName("Time"), flagName = getEctdColName("Missing"), ...)
}

\arguments{
  \item{data}{ (Required) Data frame to which to add a "missingness" flag }
  \item{dropFunc}{ (Required) Drop out function to apply to the data.  The function must return a vector of booleans }
  \item{seed}{ (Optional) Random generation seed.  By default, the current random seed is used }
  \item{idCol}{ (Optional) The name of the subject varuable in the data.  "SUBJ" by default }
  \item{timeCol}{ (Optional) The name of the time column in the data.  "TIME" by default }
  \item{flagName}{ (Optional) The name of the missing variable to create in the data.  "MISSING" by default }
  \item{\dots}{ (Optional) Extra arguments to be passed directly to the dropout function.  No arguments are passed by default }
}
\details{
  This function will accept (and check) a "dropout" function, which is to applied to the data.  This function must return a boolean vector.
  The dropout function supplied will be applied to the data provided (possibly with extra arguments supplied with the ellipses).
  Once a boolean vector is returned, the function will ensure dropout is "retained" over time within subject (so if a subject drops out at visit 1, he/she is also missing at visits 2, 3 etc.).
  This "retained" boolean vector is added to the dataset (or used to enhance an existing "missing" flag) and the updated data is returned.
}
\value{
  An updated data frame with added/updated "missing" column
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{\code{\link{checkDropOutFun}} and \code{\link{createMCAR}}}
\examples{
\dontrun{

dFun <- function(data, prop) sample(0:1, nrow(data), replace=TRUE, prob=c(1-prop, prop))
createDropout(data=myDf, dropFunc=dFun, prop=.05)
#     SUBJ TIME MISSING
#  1     1    1       0
#  2     1    2       0
#  3     1    3       0
#  4     1    4       0
#  5     1    5       0
#  6     2    1       0
#  7     2    2       0
#  8     2    3       1
#  9     2    4       1
#  10    2    5       1
#  11    3    1       0
#  12    3    2       1
#  13    3    3       1
#  14    3    4       1
#  15    3    5       1
}
}
\keyword{datagen }


