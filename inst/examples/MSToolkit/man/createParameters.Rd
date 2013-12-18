\name{createParameters}
\alias{createParameters}

\title{ Create parameter data for subjects in a simulated trial  } 
\description{
  Creates parameter effects data for each subject in a simulated trial, either based on an external file or generated based on a multivariate normal distribution
}
\usage{
createParameters(subjects, genNames, genFixedMean, genFixedCov = 0, genRange, 
genBetweenNames, genBetweenMean, genBetweenCov, genErrStruc = "None", 
genMaxDraws = 10, genParRangeTolerance = .5, extFile, extNames, extBetween, extBetweenNums, extSubset, extRange,
extErrStruc = "None", extRefCol, extRefColName, extRefColSuffix, extIndEffects, extDataId=idCol, workingPath = getwd(), 
suffix = ".Between", idCol = getEctdColName("Subject"), seed = .deriveFromMasterSeed(), flagName = getEctdColName("ParOmit"))
}

\arguments{
  \item{subjects}{ (Required) Vector of subjects (or number of subjects) for which to create parameters}
  \item{genNames}{ (Optional) Names of fixed effects to generate.  Default is missing}
  \item{genFixedMean}{ (Optional) Means for generating fixed parameters.  Default is missing}
  \item{genFixedCov}{ (Optional) Covariance Matrix for generating fixed parameter.  Default is a matrix of zeros }
  \item{genRange}{ (Optional) Range of Acceptable values for derived fixed and random effects.  Default is missing}
  \item{genBetweenNames}{ (Optional) Names of between subjects effects to generate (must be a subset of the "genNames" input).  Default is missing}
  \item{genBetweenMean}{ (Optional) Means for generated between subject effects.  Default is missing}
  \item{genBetweenCov}{ (Optional) Between subject effect covariance matrix.  Default is missing}
  \item{genErrStruc}{ (Optional) Error structure for mapping generated fixed and between subject effects: Additive, Proportional, Log-Normal or None.  Default is "None" }
  \item{genMaxDraws}{ (Optional) Maximum number of draws to take when trying to generate values within a specified range.  Default is 10 }
  \item{genParRangeTolerance}{ (Optional) Percentage of subjects with acceptable inputs for which we should continue }
  \item{extFile}{ (Optional) Name of external file from which to import parameter data.  Default is missing}
  \item{extNames}{ (Optional) Names of parameters to import from the external data file.  Default is missing}
  \item{extBetween}{ (Optional) Between subject effect variables in the data.  Default is missing}
  \item{extBetweenNums}{ (Optional) Integer mapping between imported fixed and "between subject" effects.  Default is missing}
  \item{extSubset}{ (Optional) Subset to be applied to imported data before sampling.  Default is missing}
  \item{extRange}{ (Optional) Range of acceptable values for final external parameters }
  \item{extErrStruc}{ (Optional) (None) Error structure for mapping imported fixed and between subject effects: Additive, Proportional, Log-Normal or None.  Default is "None" }
  \item{extRefCol}{ (Optional) Column of reference in imported data.  Default is missing}
  \item{extRefColName}{ (Optional) Reference column name in imported data.  Default is missing}
  \item{extRefColSuffix}{ (Optional) Reference column name suffix in imported data }
  \item{extIndEffects}{ (Optional) Individual effects flag: do the external fixed effects represent individualised parameters.  Default is missing}
  \item{extDataId}{ (Optional) Subject variable name in external data file.  Default is same as "idCol" input }
  \item{workingPath}{ (Optional) Working directory from which to import external parameter data.  By default, the current working directory is used }
  \item{suffix}{ (Optional) Suffix to use for retained "between subject" effects.  By default, ".Between" is used as the suffix }
  \item{idCol}{ (Optional) Subject variable name ("SUBJ" by default) }
  \item{seed}{ (Optional) Random number generate seed.  By default, this is derived from the current random seed }
  \item{flagName}{ (Optional) Name for "parameter omit" flag ("PAROMIT" by default) }
}
\details{
  This function is a wrapper for 2 lower level functions: \code{\link{createNormalParameters}} and \code{\link{createExternalParameters}} 
  The function passes the "gen" arguments to \code{\link{createNormalParameters}} and checks the return structure.
  It then passed the "ext" arguments to \code{\link{createExternalParameters}} and again checks the return structure.
  If only one of \code{\link{createNormalParameters}} or \code{\link{createExternalParameters}} are called, the single data structure is returned.
  If both \code{\link{createNormalParameters}} and \code{\link{createExternalParameters}} have been called, the inputs are combined into a single dataset for return.
}
\value{
  A data frame with a subject variable, a "parameter omit" flag and derived/imported parameter variables
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ \code{\link{createNormalParameters}} and \code{\link{createExternalParameters}} }
\examples{\dontrun{

# Generated parameters only
createParameters( subjects = 50, genNames = "X,Y,Z", genFixedMean = "0,0,0", genFixedCov = "1", 
  genBetweenNames = "X,Y,Z", genBetweenCov = "1", genBetweenMean = "0,0,0", genErrStruc = "a" )

# Imported parameters only
createParameters( subjects = 50, extNames = "E0,ED50,EMAX", extFile = "testParam.csv", 
  extErrStruc = "Additive", extBetween = "B1,B2", extBetweenNums = "1,3")

# Both generated and imported parameters
createParameters( subjects = 50, 
  extNames = "E0,ED50,EMAX", extFile = "testParam.csv", extErrStruc = "Additive", extBetween = "B1,B2", extBetweenNums = "1,3", 
  genNames = "X,Y,Z", genFixedMean = "0,0,0", genFixedCov = "1", genBetweenNames = "X,Y,Z", genBetweenCov = "1", 
  genBetweenMean = "0,0,0", genErrStruc = "a" )
  
}}

\keyword{ datagen }

