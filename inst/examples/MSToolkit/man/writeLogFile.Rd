\name{writeLogFile}
\alias{writeLogFile}
\title{Write a job log file}
\description{Writes a log file of the status of all jobs that are currently executing on the grid.  If all jobs are done, the log
will indicate which ran successfully and which did not.  If not, the log file will show the amount of time all jobs have been
running, along with the number of jobs of each status.
}

\usage{
writeLogFile(jobStatuses, startingTime, logFileName = "jobstatus.log", 
  statusNames = c("PEND", "RUN", "DONE", "EXIT", "SUSPEND", "UNKWN"), 
  workingPath = getwd()) 
}

\arguments{
  \item{jobStatuses}{ (Required) A vector of strings of statuses of all currently running jobs }
  \item{startingTime}{ (Required) A time at which the jobs started running }
  \item{logFileName}{ (Optional) Name of the file to write to ("jobstatus.log" by default) }
  \item{statusNames}{ (Optional) A vector of strings that contain all possible names of statuses that jobs could be in.  By default, this is "PEND", "RUN", "DONE", "EXIT", "SUSPEND" and "UNKWN" }
  \item{workingPath}{ (Optional) The working directory in which to create the log file.  By default, the current working directory is used }
}

\value{
  None.
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{
 \dontrun{ 
    jstatuses <- sample(c("PEND", "RUN", "DONE", "EXIT"), 10, replace = TRUE)
    writeLogFile(jstatuses, Sys.time())
    file.show( "jobstatus.log" )
  }
}
\keyword{ IO }

