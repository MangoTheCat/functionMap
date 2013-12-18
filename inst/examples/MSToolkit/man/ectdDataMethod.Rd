\name{setEctdDataMethod}
\alias{setEctdDataMethod}
\alias{getEctdDataMethod}
\title{ Current data storage method }
\description{
Gets and sets the current "data storage" method, used for the storage of simulated trial data
}
\usage{

getEctdDataMethod()
setEctdDataMethod(method) 
}
\arguments{
  \item{method}{ (Required) The data storage method to use (either 'CSV', 'RData' or 'Internal'}
}
\details{
Gets (getEctdDataMethod) and sets (setEctdDataMethod) the current "data storage" method, used for the storage of simulated trial data.
The choices of storage method are:
* CSV - Replicate data stored in seperate CSV files outside of R
* RData - Replicate data stored in seperate RData files outside of R
* Internal - Replicate data stored as a list of data frames in an internal environment (.ectdEnv$DataStore)

Note: The data storage only impacts replicate data - micro and macro evaluation data is always held as CSV files

}
\value{
The "getEctdDataMethod" function returns the current data method ("CSV", "RData" or "Internal")
The "setEctdDataMethod" function invisibly returns the method that has just been set as the default
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{\dontrun{

	nowMethod <- getEctdDataMethod()
	
	setEctdDataMethod("CSV")
	getEctdDataMethod()
	
	setEctdDataMethod("RData")
	getEctdDataMethod()
	
	setEctdDataMethod("Internal")
	getEctdDataMethod()

	setEctdDataMethod(nowMethod)
	
}}
\keyword{ IO }

