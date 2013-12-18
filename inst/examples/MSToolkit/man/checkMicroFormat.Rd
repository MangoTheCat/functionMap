\name{checkMicroFormat}
\alias{checkMicroFormat}

\title{Check that an input data structure is a valid "Micro Evaluation" structure} 
\description{\code{checkMicroFormat} checks that a given data frame meets certain criteria and returns either a modified version
of that data frame or an "empty" micro evaluation frame with given dose data and column names.  See the details section below
for more information. 
}

\usage{
checkMicroFormat(data, doseCol = getEctdColName("Dose"), mustHaveDose = FALSE)
}

\arguments{
  \item{data}{ (Required) A data frame to check for validity }
  \item{doseCol}{ (Optional) The "Dose" variable name within the supplied data.  "DOSE" by default }
  \item{mustHaveDose}{ (Optional) Should the "doseCol" variable necessarily be present?  Default FALSE }
}
\details{

\code{checkMicroFormat} will perform a series of basic checks on \code{data}.  

The checks for a valid structure are as follows: 
(1) \code{data} is a data frame
(2) \code{data} has at least 1 row
(3) if "mustHaveDose" is TRUE, \code{data} contains a "Dose" column (given by \code{doseCol})

If any of these fail it will stop or return a NULL structure

}

\value{A data frame as described above}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{\code{\link{createEmptyMicro}}}
\examples{
  
  checkMicroFormat(data = data.frame(DOSE = 1:4), doseCol = "DOSE", mustHaveDose = TRUE)

}
\keyword{datagen}

