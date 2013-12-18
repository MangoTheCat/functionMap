\name{createNmParSamples}
\alias{createNmParSamples}
\title{Simulate parameter sets from a NONMEM run object}
\description{
	Creates a list containing a set of "THETA", "OMEGA" and "SIGMA" parameter values, based on either the initial or final estimates of a NONMEM run
}
\usage{
createNmParSamples(N, run, seed = .deriveFromMasterSeed(), 
	method = c("Covariance", "Final", "Initial")) 
}
\arguments{
        \item{N}{ Number of samples to take } 
        \item{run}{ Either a NONMEM run, NONMEM control file, or NONMEM output file, as imported using \emph{RNMImport} } 
        \item{seed}{ Random number seed } 
        \item{method}{ Parameter method: Covariance, Final or Initial}
}
\details{
Firstly, the function extracts the information needed to simulate.
* If method is "Covariance", it extracts the final estimates and covariance matrix from the run
* If method is "Final", it extracts the final estimates from the run
* If method is "Initial", it extracts the initial estimates from the run

If method is either "Final" or "Initial", then the parameters selected are just repeated "N" times and returned as a single list structure (ie. no difference between parameter sets)

If method is "Covariance" and the covariance matrix can be extracted, samples are taken from a multivariate normal distribution (using the \link{mvrnorm} function in the \emph{MASS} library).  These samples are formatted in a list and returned

}
\value{
	A list of length "N", where each element of the list contains:
	* An element called "THETA", containing a vector of fixed effects
	* An element called "OMEGA", containing a matrix of between subject effects
	* An element called "SIGMA", containing a matrix of within subject effects
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\keyword{ nonmem }
