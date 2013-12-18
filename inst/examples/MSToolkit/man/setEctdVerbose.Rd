\name{setEctdVerbose}
\alias{setEctdVerbose}
\alias{setEctdLogFile}
\alias{setEctdDateFormat}
\alias{getEctdVerbose}
\alias{getEctdLogFile}
\alias{getEctdDateFormat}
\title{ MSToolkit package options }
\description{
  Options used by the MSToolkit package to control the logfile, the amount of messages
  that are written in the logfile, and the format of the date. 
}
\usage{

 setEctdVerbose(verbose)
 setEctdLogFile(file)
 setEctdDateFormat(format)

 getEctdVerbose()
 getEctdLogFile()
 getEctdDateFormat()

}

\arguments{
  \item{verbose}{ (Required) A logical value.  If set to TRUE, messages are sent to the logfile during the process of generating the data and analyzing it. Set to \code{TRUE} when attaching the package.}
  \item{file}{ (Required) Log File to write messages in.  It is eventually use by \code{\link{cat}}, so \code{file=""} will send the output messages back to the standard output. Set to \code{"ectd.log"} when attaching the package. }
  \item{format}{ (Required) Format for the date. See \code{\link{strptime}}. Set to a default value when the package is attached.}
}

\details{
  The three function write and read information from the (not exported) environment \code{.ectdEnv}.
  These settings are mainly used by the (not exported) \code{.log} function.
}
\value{
  The function (invisibly) returns the previous value of the arguments.
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ \code{\link{options}} provides a similar mechanism for R options.}
\examples{\dontrun{
  oldverb <- setEctdVerbose( TRUE )
  olddf   <- setEctdDateFormat("%Y")
  oldlf   <- setEctdLogFile("mstoolkit.log")
  
  for( i in 1:100 ) {
    MSToolkit:::.log( paste("some message:", i) )
  }
  file.show( getEctdLogFile() )
  
  setEctdVerbose   (oldverb)
  setEctdDateFormat(olddf  )
  setEctdLogFile   (oldlf  )
 
}}
\keyword{ IO }

