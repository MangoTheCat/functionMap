\name{createNormalParameters}
\alias{createNormalParameters}

\title{ Create parameters based on a multivariate normal distribution } 
\description{
  Creates a set of fixed (and optionally between subject) parameter variables by sampling from a multivariate normal distribution
}
\usage{
createNormalParameters(subjects, names, mean, covariance = 0, range, betNames, betMean = 0, betCov = 0, 
errStruc = c("None", "Proportional", "Additive", "Log-Normal"), suffix = ".Between", idCol = getEctdColName("Subject"), 
maxDraws = 10, seed = .deriveFromMasterSeed(), flagName = getEctdColName("ParOmit"), digits = 3, parRangeTolerance = 0.5)
}

\arguments{
  \item{subjects}{ (Required) Subjects for which to create parameters }
  \item{names}{ (Required) Names of parameters to generate }
  \item{mean}{ (Required) Means for fixed parameters }
  \item{covariance}{ (Optional) Covariance matrix for fixed parameters.  By default, this is a matrix of zeros, representing no error to be included }
  \item{range}{ (Optional) Range of acceptable values.  Missing by default, resulting in no "range" of values is applied to the data }
  \item{betNames}{ (Optional) Between subject effects to create.  Default is to not create between subject data }
  \item{betMean}{ (Optional) Means for the between subject effects.  Default is to not create between subject data  }
  \item{betCov}{ (Optional) Covariance matrix for the between subject effects.  Default is to not create between subject data  }
  \item{errStruc}{ (Optional) (None) Function to map between subject effects: Additive, Proportional, Log-Normal or None }
  \item{suffix}{ (Optional) Suffix to use for retain between subject effects (default is ".Between") }
  \item{idCol}{ (Optional) Subject variable name for return data ("SUBJ" by default) }
  \item{maxDraws}{ (Optional) Maximum number of iterations for valid parameters (10 by default) }
  \item{seed}{ (Optional) Random number generation seed.  By default, this is dervied from the current random seed }
  \item{flagName}{ (Optional) Flag name for parameters out of bounds ("PAROMIT" by default) }
  \item{digits}{ (Optional) Number of digits to which to round generated continuous parameters.  Can be an atomic integer, in which case all variables are rounded according to it. Can be a numeric vector of the same length of the number of fixed parameters, in which case each parameter is rounded according to its value.  The default is 3 }
  \item{parRangeTolerance}{ (Optional, default .9) Proportion of subjects with parameters in specified range that is acceptable for us to continue }
}
\details{
  The function will first create a single set of fixed effects, to be repeated for each subject specified.  This single set of fixed effects will be taken from a multivariate normal distribution with mean and covariance matrix supplied.
  If the "betNames" input has been suppied, and the "betNames" vector is a subset of the fixed effect names, between subject effects will be derived.
  If the "betMean" input has not been supplied, the mean of the between effects distribution will be 0.  Samples for each subject will be taken from a multivariate normal distribution with mean "betMean" and covariate matrix  "betCov".

  If fixed and between subject effects have been generated, the "errStruc" input will specify how the between subject effects are to be applied:
  * None = Keep between subject effect seperate (and use the "suffix" supplied to name the variables)
  * Additive = Add each between subject effect to the corresponding fixed effect
  * Proportional = Multiply the fixed effect by the exponentiated between subject effect
  * Log-Normal = Add "1 + each between subject effect" to the corresponding fixed effect

  If the "range" argument is provided, the output from the above algorithm (fixed + between) is checked against the acceptance range.
  If the data is not within range, the function will take "maxDraws" more attempts at the above algorithm to try and generate "in range" data.  If that is not possible, the function will stop and return an error.
}
\note{
If earlier versions of MSToolkit, the "Proportional" error structure was implemented as exp(fixed + between subject)
}
\value{
  A data frame containing parameter data
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }

\seealso{ \code{\link{createParameters}} and \code{\link{createExternalParameters}}  }
\examples{
\dontrun{
createNormalParameters(5, "E0,ED50,EMAX", mean = c(0, 50, 100), covariance = diag(c(1, 5, 10)))
#  SUBJ       E0     ED50     EMAX   PAROMIT
#1    1 -0.8356286 50.41064 98.01898       0
#2    2 -0.8356286 50.41064 98.01898       0
#3    3 -0.8356286 50.41064 98.01898       0
#4    4 -0.8356286 50.41064 98.01898       0
#5    5 -0.8356286 50.41064 98.01898       0
createNormalParameters(5, "E0,ED50,EMAX", mean = c(0, 50, 100), covariance = diag(c(1, 5, 10)), 
betNames = c("E0", "EMAX"), betCov = diag(2), errStruc = "Additive")
#  SUBJ       E0     ED50     EMAX   PAROMIT
#1    1  0.7596522 50.41064 98.59476       0
#2    2 -0.5061208 50.41064 97.71359       0
#3    3 -1.6560970 50.41064 99.53076       0
#4    4 -0.3481996 50.41064 98.40882       0
#5    5 -0.0973039 50.41064 97.39774       0

% added for SF issue 8
% Tue Jul 24 10:35:42 BST 2007 @441 /Internet Time/
# no covariance by default 
createNormalParameters(5, "E0,ED50,EMAX", mean = c(0, 50, 100) )  
#  SUBJ E0 ED50 EMAX PAROMIT
#1    1  0   50  100       0
#2    2  0   50  100       0
#3    3  0   50  100       0
#4    4  0   50  100       0
#5    5  0   50  100       0

}
}

\keyword{ datagen }

