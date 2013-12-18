\name{ectdStop}
\alias{ectdStop}
\alias{ectdWarning}

\title{ Error handling functions for the ectd package. } 
\description{
   Error handling functions for the ectd package.
}
\usage{
ectdStop(msg, call. = TRUE, domain = NULL, verbose = getEctdVerbose() )

ectdWarning(..., call. = TRUE, immediate. = FALSE, domain = NULL)
}

\arguments{
  \item{msg}{ (Required) Message to be displayed to the user }
  \item{verbose}{ (Optional) Should more details be given when showing the error message/warning.  By default, this is controlled by support function \link{getEctdVerbose} }
  \item{call., domain, immediate.}{  See \code{\link{warning}} and \code{\link{stop}}. }
  \item{\dots}{passed to warning}
}
\details{
  These functions are alias to the standard R functions stop and warning, but they also 
  do character interpolation. See examples.
}
\value{
  Nothing. 
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ See \code{\link{warning}} and \code{\link{stop}}.  }
\examples{
  
  \dontrun{
     ectdStop("something wrong happened")
  }
  
}
\keyword{ error }

