\name{setEctdExternalPath}
\alias{setEctdExternalPath}
\alias{getEctdExternalPath}
\title{ Controls paths to a set of external execution paths }
\description{
	Gets and sets paths to external 
}
\usage{
getEctdExternalPath(pathName)
setEctdExternalPath(pathName, Value) 
}
\arguments{
  \item{pathName}{ Name of the path to return or set.  When using getEctdExternalPath, leave pathName blank to return a vector of available path names }
  \item{Value}{ New value for the path }
}

\details{

getEctdExternalPath gets the execution path for a specific application on a particular environment
setEctdExternalPath sets an execution path for an application

More permanent changes can be made by modifying the "ECTD.ini" file in the library root

}
\value{
The "getEctdExternalPath" function returns the current execution path for a given "pathName"
The "setEctdExternalPath" function invisibly returns the updated path list

}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{\dontrun{

	getEctdExternalPath()		# Look at available paths

	getEctdExternalPath("SASPATH_WIN")		# Get the "SAS Execution on Windows" path
		
}}
\keyword{ IO }
