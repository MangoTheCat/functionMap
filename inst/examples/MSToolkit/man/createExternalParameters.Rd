\name{createExternalParameters}
\alias{createExternalParameters}
\title{Sample parameters from an external file}
\description{
  Create parameters for a simulated population by sampling them from an external file
}
\usage{
  createExternalParameters(subjects, file, names, range, betNames, betNums, 
  errStruc = c("None", "Proportional", "Additive", "Log-Normal"), 
  subset = NULL, refCol, refColName, idCol = getEctdColName("Subject"), 
  seed = .deriveFromMasterSeed(), flagName = getEctdColName("ParOmit"), refColSuffix = "refCol", 
  dataId = idCol, workingPath = getwd())
}
\arguments{
  \item{subjects}{ (Required) Number of subjects for which to create parameters }
  \item{file}{ (Required) External file from which to extract parameters }
  \item{names}{ (Required) Names of parameters to import (refers to variables in the input data) }
  \item{range}{ (Optional) Range of acceptable values.  Missing by default, resulting in no "range" of values is applied to the data }
  \item{betNames}{ (Required) Between subject effects variables in the data }
  \item{betNums}{ (Required) Integer vector mapping between subject effects onto fixed effects }
  \item{errStruc}{ (Optional) (None) Function to map between subject effects: Additive, Proportional, Log-Normal or None }
  \item{subset}{ (Optional) Subset to apply to the data.  By default, no subset is applied }
  \item{refCol}{ (Optional) Vector of data to be matched to the reference column for sampling (no reference data by default) }
  \item{refColName}{ (Optional) Column name in data for referenced sampling (no reference column by default) }
  \item{idCol}{ (Optional) Subject variable name in the data ("SUBJ" by default) }
  \item{seed}{ (Optional) Random number seed.  Derived from the current seed by default }
  \item{flagName}{ (Optional) Name for "parameter omit" flag ("PAROMIT" by default) }
  \item{refColSuffix}{ (Optional) Suffix to add to reference variable in the data ("refCol" by default) }
  \item{dataId}{ (Optional) The subject variable name in the data.  By default, this is taken to be the same as the "idCol" input }
  \item{workingPath}{ (Optional) Working path from which to import covariate file.  By default, the working path will be used }
}
\details{
  The data is first imported from the specified file, and any subset applied to it.  The function next checks that all required variables specifed appear in the imported data.
  Fixed effects (specified using names) are selected from the file along with between subject effects (if they are also identified).

  If fixed and between subject effects have been generated, the "errStruc" input will specify how the between subject effects are to be applied:
  * None = Keep between subject effect seperate (and use the "suffix" supplied to name the variables)
  * Additive = Add each between subject effect to the corresponding fixed effect
  * Proportional = Multiply the fixed effect by the exponentiated between subject effect
  * Log-Normal = Add "1 + each between subject effect" to the corresponding fixed effect
  
  If the relCol input is specified, this variable should also be retained in the result.
  The "parameter omit" flag will contain repeated "0" elements in the return result.
}
\note{
If earlier versions of MSToolkit, the "Proportional" error structure was implemented as exp(fixed + between subject)
}
\value{
  A data frame containing a subject variable, a "parameter omit flag" and a set of extracted parameters
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ 
  \code{\link{createParameters}} and \code{\link{createNormalParameters}}
}
\examples{
\dontrun{
  wPath <- system.file( "Runit", "data", "createParameters", package = "MSToolkit" ) 
 
  createExternalParameters( subjects = 100, names = "E0,ED50", 
    file = "testParam.csv", errStruc = "None", betNames = "B1,B2", workingPath = wPath )
}}

\keyword{ datagen }

