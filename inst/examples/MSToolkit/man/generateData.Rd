\name{generateData}
\alias{generateData}

\title{ Generate simulated data replicates } 
\description{
  Generate Simulated Data Replicates by controlling dosing, covariates, parametes, response, missingness and interims
}
\usage{
generateData(replicateN, subjects = NULL, treatSubj = subjects, treatDoses, treatSeq, treatType = "Parallel", treatPeriod, 
  genParNames, genParMean, genParVCov = 0, respEqn, respName = getEctdColName("Response"), treatProp, treatOrder = FALSE, conCovNames, conCovMean, conCovVCov, conCovCrit = NULL, 
  conCovDigits = 3, conCovMaxDraws = 100, disCovNames, disCovVals, disCovProb, disCovProbArray, extCovNames, extCovFile, extCovSubset, extCovRefCol, extCovSameRow = TRUE, extCovDataId = idCol, 
  timeCovNames , timeCovMean, timeCovVCov, timeCovCrit = NULL, genParCrit, genParBtwNames, genParBtwMean, genParBtwVCov, genParErrStruc = "None", 
  genParMaxDraws = 100, genParRangeTolerance = .5, extParFile, extParNames, extParBtwNames, extParBtwNums, extParSubset = NULL, extParCrit, extParErrStruc = "None", extParRefColData, 
  extParRefColName, extParDataId = idCol, respInvLink, respDist = "Normal", respVCov, respErrStruc = "Additive", respCrit, respDigits = 3, mcarProp = 0, mcarRule, dropFun,
  dropFunExtraArgs = list(), interimSubj, interimMethod = "Sample", seed = .deriveFromMasterSeed(), idCol = getEctdColName("Subject"), doseCol = getEctdColName("Dose"), timeCol = getEctdColName("Time"), 
  trtCol = getEctdColName("Trt"), parOmitFlag = getEctdColName("ParOmit"), respOmitFlag = getEctdColName("RespOmit"), missingFlag = getEctdColName("Missing"), interimCol = getEctdColName("Interim"), parBtwSuffix = ".Between", 
  deleteCurrData = TRUE, covDiff = TRUE, treatDiff = TRUE, workingPath = getwd() )
}

\arguments{
  \item{replicateN}{ (Required) Number of replicates for which to create simulated data }
  \item{subjects}{ (Required) Number of subjects in simulation }
  \item{treatSubj}{ (Optional) Number of subjects to which to allocate treatments, or a vector of allocations }
  \item{treatDoses}{ (Optional) Vector of numeric treatment doses.  By default, this is the same as the "subjects" input }
  \item{treatSeq}{ (Optional) Treatment matrix for crossover designs.  Missing by default, but this is required when treatType is set to "Crossover" }
  \item{treatType}{ (Optional) Treatment type: 'Parallel' or 'Crossover'.  Default is "Parallel" }
  \item{treatPeriod}{ (Optional) Vector of numeric treatment time points.  Missing by default, resulting in no "time" element in the generated data }
  \item{genParNames}{ (Optional) Names of fixed effects to generate.  Missing by default, resulting in no fixed parameters being created }
  \item{genParMean}{ (Optional) Means for generating fixed parameters.  Missing by default }
  \item{genParVCov}{ (Optional) Covariance matrix for generating fixed parameters.  By default, this is a matrix of zeros }
  \item{respEqn}{ (Required) Formula for creating the simulated response }
  \item{respName}{ (Optional) Response variable name.  Default is "RESP" }
  \item{treatProp}{ (Optional) Proportions for sampling.  Missing by default, resulting in unbiased sampling }
  \item{treatOrder}{ (Optional) Logical flag: should allocations be assigned in order.  FALSE by default }
  \item{conCovNames}{ (Optional) Continuous covariate names.  Missing by default, resulting in no continuous covariates being created }
  \item{conCovMean}{ (Optional) Continuous covariate means.  Missing by default }
  \item{conCovVCov}{ (Optional) Continuous covariate covariance matrix.  Missing by default }
  \item{conCovCrit}{ (Optional) Continuous covariate acceptable range.  Missing by default }
  \item{conCovDigits}{ (Optional) Continuous covariate rounding digits.  3 by default }
  \item{conCovMaxDraws}{ (Optional) Continuous covariate maximum draws.  100 by default }
  \item{disCovNames}{ (Optional) Discrete covariate names.  Missing by default, resulting in no discrete covariates being created }
  \item{disCovVals}{ (Optional) Discrete covariate values.  Missing by default }
  \item{disCovProb}{ (Optional) Discrete covariate probabilities.  Missing by default }
  \item{disCovProbArray}{ (Optional) Array of probabilities for multivariate sampling.  Missing by default }
  \item{extCovNames}{ (Optional) Names for the continuous covariates.  Missing by default, resulting in no imported covariates }
  \item{extCovFile}{ (Optional) File from which to import (including full or relative path).  Missing by default }
  \item{extCovSubset}{ (Optional) Subset to apply to data.  Missing by default }
  \item{extCovRefCol}{ (Optional) Reference variable.  Missing by default }
  \item{extCovSameRow}{ (Optional) Logical flag: should covariates sampled be from the same row.  TRUE by default }
  \item{extCovDataId}{ (Optional) Subject variable name from file.  Same as "idCol" by default }
  \item{timeCovNames}{ (Optional) Time-varying covariate names.  Missing by default, resulting in no Time-varying covariates being created }
  \item{timeCovMean}{ (Optional) Time-varying covariate means.  Missing by default }
  \item{timeCovVCov}{ (Optional) Time-varying covariate covariance matrix.  Missing by default }
  \item{timeCovCrit}{ (Optional) Time-varying covariate acceptable range.  Missing by default }
  \item{genParCrit}{ (Optional) Range of acceptable values for generated fixed effects.  Missing by default }
  \item{genParBtwNames}{ (Optional) Between subject effects to generate.  Missing by default, resulting in no created between subject effects }
  \item{genParBtwMean}{ (Optional) Means for generated between subject effects.  Missing by default }
  \item{genParBtwVCov}{ (Optional) Covariance matrix for generated between subject effects.  Missing by default }
  \item{genParErrStruc}{ (Optional) Function to map generated effects: Additive, Proportional or None.  "None" by default }
  \item{genParMaxDraws}{ (Optional) Maximum number of iterations to generate valid parameters.  100 by default}
  \item{genParRangeTolerance}{ (Optional) Proportion of subjects with "in range" parameter data that we're happy proceeding with }
  \item{extParFile}{ (Optional) File name for external parameter data to import.  Missing by default, resulting in no imported parameter variables }
  \item{extParNames}{ (Optional) Names of parameters to import from external file.  Missing by default }
  \item{extParBtwNames}{ (Optional) Between subject effects variables to import from external file.  Missing by default }
  \item{extParBtwNums}{ (Optional) Integer mapping between random and fixed effects in imported parameter data.  Missing by default }
  \item{extParSubset}{ (Optional) Subsets to be applied to imported parameter before sampling.  Missing by default }
  \item{extParCrit}{ (Optional) Acceptance range for imported parameter columns }
  \item{extParErrStruc}{ (Optional) Function to map effects from imported parameter data: Additive, Proportional or None.  "None" by default }
  \item{extParRefColData}{ (Optional) Reference column in imported parameter data.  Missing by default }
  \item{extParRefColName}{ (Optional) Reference column name from imported parameter data.  Missing by default }
  \item{extParDataId}{ (Optional) Subject variable name in external parameter file.  Same as "idCol" by default }
  \item{respInvLink}{ (Optional) Inverse link function for the linear predictor.  Missing by default, resulting in no inverse link to be applied }
  \item{respDist}{ (Optional) Outcome response variable distribution ("Normal" by default) }
  \item{respVCov}{ (Optional) Residual error (co)variance to apply to generated response.  None by default }
  \item{respErrStruc}{ (Optional) Function describing how to apply residual error to the generated response: Additive, Log-Normal or Proportional.  "Additive" by default }
  \item{respCrit}{ (Optional) Range of acceptable values for created response.  Missing (no criteria) by default }
  \item{respDigits}{ (Optional) Number of digits to which to round the created response.  3 by default }
  \item{mcarProp}{ (Optional) Proportion of observations to set to missing at random.  0 by default }
  \item{mcarRule}{ (Optional) Rule to specify which observations of the data should be included for MCAR allocation.  Missing by default }
  \item{dropFun}{ (Optional) User defined function to define criteria for subject dropout.  Missing (no dropout) by default }
  \item{dropFunExtraArgs}{ (Optional) Additional arguments to the dropout function.  None by default }
  \item{interimSubj}{ (Optional) Proportion of total subjects to be assigned to each interim analysis.  Missing by default, resulting in no "interim" variable derived }
  \item{interimMethod}{ (Optional) Method for creating interim variable: 'Sample' or 'Proportion'.  "Sample" by default }
  \item{seed}{ (Optional) Random seed.  By default, this is derived from the current session random seed }
  \item{idCol}{ (Optional) Subject variable name ("SUBJ" by default) }
  \item{doseCol}{ (Optional) Dose variable name ("DOSE" by default) }
  \item{timeCol}{ (Optional) Time variable name ("TIME" by default) }
  \item{trtCol}{ (Optional) Treatment variable name ("TRT" by default) }
  \item{parOmitFlag}{ (Optional) Parameter omit flag name ("PAROMIT" by default) }
  \item{respOmitFlag}{ (Optional) Response omit flag name ("RESPOMIT" by default) }
  \item{missingFlag}{ (Optional) Missingness flag name ("MISSING" by default) }
  \item{interimCol}{ (Optional) Interim variable name ("INTERIM" by default) }
  \item{parBtwSuffix}{ (Optional) Suffix for retained between subject effects variables.  Suffix ".Between" is used by default }
  \item{deleteCurrData}{ (Optional) Should existing data be deleted before starting generation phase (TRUE by default) }
  \item{covDiff}{ (Optional) Should covariates differ between replicates (TRUE by default) }
  \item{treatDiff}{ (Optional) Should treatment allocation differ between replicates (TRUE by default) }
  \item{workingPath}{ (Optional) Working directory from which to create data.  By default, the current working directory is used }  
}
\details{
  
The generateData function calls the low level generate data components to create sets of simulated data.  
The following components are called to create aspects of the simulated trial data:

  \code{\link{createTreatments}}: Used to create a dataset of all possible treatment regimes to be allocated to subjects

  \code{\link{allocateTreatments}}: Use to allocate treatments to subjects in the simulated study

  \code{\link{createCovariates}}: Creates a set of fixed covariates for a simulated population

  \code{\link{createParameters}}: Creates simulated fixed and between subject parameters for subjects in each replicate

  \code{\link{createResponse}}: Creates a simulated response variable based on available derived data

  \code{\link{createMCAR}}: Adds a simulated "missing" flag to the data 

  \code{\link{createDropout}}: Adds a simulated "missing" flag to the data based on a dropout function 

  \code{\link{createInterims}}: Assigns subjects in the study to interim analyses

The function iteratively builds and combines the data components for each replicte, and stores the data in the "ReplicateData" subdirectory of the working directory.  
This data can then be analyzed using a call to the \code{\link{analyzeData}} function.

}
\value{
  No value is returned from the generateData function.  However, as a side effect, a number of simulated replicate datasets are created.
}

\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ 
  \code{\link{createTreatments}}, \code{\link{allocateTreatments}}, \code{\link{createCovariates}}, \code{\link{createParameters}}, 
  \code{\link{createResponse}}, \code{\link{createMCAR}}, \code{\link{createDropout}}, \code{\link{createInterims}} and \code{\link{analyzeData}}
}
\examples{

\dontrun{
generateData( replicateN = 500, subjects = 400, treatDoses = c(0, 5, 25, 50, 100), 
  conCovNames = c("wt", "age"), conCovMean = c(83, 55) , conCovVCov = c(14,10)^2 , 
  conCovDigits = 1, conCovCrit = "18 <= age <= 65", 
  genParNames = "E0,ED50,EMAX", genParMean = c(2,50,10), genParVCov = diag( c(.5,30,10) ), 
  genParBtwNames = "E0,ED50,EMAX", genParBtwMean = c(0,0,0), genParBtwVCov = diag(3), 
  respEqn = "E0 + ((DOSE * EMAX)/(DOSE + ED50))",  respVCov = 5, 
  interimSubj = ".3,.7")
}
}


\keyword{ datagen }

