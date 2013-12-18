\name{createExternalCovariates}
\alias{createExternalCovariates}
\title{ Create covariates by sampling from an external file }
\description{
  Create covariates by sampling from an external file.
}
\usage{
createExternalCovariates(subjects, names, file, sameRow = TRUE, 
  subset = NULL, refCol = NULL, dataId = idCol, idCol = getEctdColName("Subject"), 
  percent = 20, seed = .deriveFromMasterSeed(), includeIDCol = TRUE, 
  refColSuffix = "refCol", workingPath = getwd())
}
\arguments{
  \item{subjects}{ (Required) Number of subjects for which to sample sets of external covariate values }
  \item{names}{ (Required) Names of the covariates to use from the \code{file} }
  \item{file}{ (Required) Input file name.  This should be either a valid csv file or a NONMEM data file containing all the variables given by \code{names}, \code{idCol} }
  \item{sameRow}{ (Optional) A logical value).  Should all the covariates be sampled from the same rows or should the sampling be done independantly for each covariate.  Using \code{sameRow = TRUE} would maintain the multivariate structure of the imported dataset and is faster.  TRUE by default }
  \item{subset}{ (Optional) Any subset to be performed on the imported dataset before doing any sampling. The subset is parsed by the \code{\link{parseRangeCode}} function.  No subsetting is performed by default }
  \item{refCol}{ (Optional) The reference column in the data file.  If given, the output dataset will also  contain an additional column indicating the origin of each row from the original dataset. This option is not compatible with \code{sameRow = FALSE}.  By default, reference variables are not used }
  \item{dataId}{ (Optional) The subject variable in the input dataset, equal to \code{idCol} by default.  By default, it will be the same as "idCol" }
  \item{idCol}{ (Optional) The subject variable in the output dataset.  "SUBJ" by default }
  \item{percent}{ (Optional) When a subset is performed on the input data, if the number of rows remaining in the dataset after subset is less than \code{percent} \%, the function will issue a warning.  By default, 20\% is used }
  \item{seed}{ (Optional) Random generator seed to use.  The current random seed is used by default }
  \item{includeIDCol}{ (Optional) A logical value.  Should the subject variable be included in the output dataset?  When \code{createCovariates} calls this function, it does not need the subject variable.  TRUE by default }
  \item{refColSuffix}{ (Optional) The suffix to use when creating the \code{refCol} variable.  If the \code{refCol} variable is "SUBJ", then in the output dataset it will be created as "SUBJ" suffixed with "refColSuffix".  By default, "refCol" is used as the suffix }
  \item{workingPath}{ (Optional) Working path from which to import covariate file.  By default, the current working directory is used }
}
\details{
  The sampling is always done with replacement.
  
  The \code{refCol} setting is typically used with the parameter component, 
  see \code{\link{createParameters}} or \code{\link{createExternalParameters}} to 
  maintain consistency between imported covariates and imported parameters.
}
\value{
  A data frame containing the imported variables, and possibly a reference variable. 
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ 
  \code{\link{createContinuousCovariates}}, 
  \code{\link{createDiscreteCovariates}}, and 
  \code{\link{createCovariates}}
}
\examples{\dontrun{
  
  # an example file from the unit tests of the MSToolkit package
  wPath <- system.file( "Runit", "data", "createCovariates", package = "MSToolkit" ) 
  
  # sample 20 subjects from the example file 
  dat <- createExternalCovariates( 20, names = "X1", 
    subset = c(".7 < X1 < .8", "-1 <= X2 <= 1"),  
    file = "testCovariates.csv", workingPath = wPath )
  print( dat )
    
  # maintaining the origin of each row  
  dat <- createExternalCovariates( 20, names = "X1, X2", 
    subset = c(".7 < X1 < .8", "-1 <= X2 <= 1"),  
    file = "testCovariates.csv", workingPath = wPath, refCol = "ID" )
  print( dat )

}}
\keyword{ datagen }
\keyword{ IO }


