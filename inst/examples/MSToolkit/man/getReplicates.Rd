\name{getReplicates}
\alias{getReplicates}
\title{ Trial replicates vector }
\description{
	Get a vector of available trial replicates, based on a specified data storage method
}
\usage{
	getReplicates(path = "ReplicateData", prefix = "replicate", 
		method = getEctdDataMethod(), workingPath = getwd())
}
\arguments{
  \item{path}{ The subdirectory (within "workingPath") containing the replicates files ('CSV' and 'RData' methods only) }
  \item{prefix}{ Prefix to used when looking for simulated data files }
  \item{method}{  Data storage method in use, either "CSV", "RData" or "Internal (given by  \link{getEctdDataMethod} by default)}
  \item{workingPath}{Root path for simulated data (default is working directory)}
}
\details{
	This function looks for existing replicates based on the method, directories and file prefix given.

	If method is "CSV", the function looks for files starting with "prefix" and ending in ".csv" within the "path" subdirectory of "workingPath"
	Similarly, if method is "RData", the function looks for files starting with "prefix" and ending in ".rdata" in the same location
	If method is "Internal", the function looks for data frames found in the internal data storage location (.ectdEnv$DataStore)
	
	The function will return a vector of replicate numbers, or fail if no replicates are found

}
\value{
	A numeric vector of replicate values
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{
  
  \dontrun{
	 getReplicates()
  }  
}
\keyword{ Replicates }

