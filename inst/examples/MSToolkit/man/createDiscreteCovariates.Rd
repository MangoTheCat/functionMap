\name{createDiscreteCovariates}
\alias{createDiscreteCovariates}
\title{ Create covariates from a discrete distribution }
\description{
Create covariates from a discrete distribution 
}
\usage{
createDiscreteCovariates(subjects, names, values, probs, probArray, 
  seed = .deriveFromMasterSeed(), idCol = getEctdColName("Subject"), includeIDCol = TRUE)
}
\arguments{
  \item{subjects}{ (Required) Vector of subjects (or number of subjects) for which to create covariates }
  \item{names}{ (Required) Names of the discrete covariates to be created.  All the names should be valid R names. See \code{link{validNames}}.}
  \item{values}{ (Required) Values that the covariates can take. See details section. }
  \item{probs}{ (Optional) Probabilities for each covariates. See details section. }
  \item{probArray}{ (Optional) Probability array for uneven sampling. See details section. }
  \item{seed}{ (Optional) Random seed to use.  By default, it is based on the current random seed }
  \item{idCol}{ (Optional) Name of the subject column.  Must be a valid R name (see \code{\link{validNames}}) and not be duplicated with any \code{names}.  "SUBJ" by default }
  \item{includeIDCol}{ (Optional) A logical value. Should the subject column be included. Typically, the \code{\link{createCovariates}} function would set this to FALSE as it does not need it.  TRUE by default }  
}
\details{
  The \code{values} and \code{probs} argument are parsed using the \code{\link{parseHashString}} helper
  function. They could be either : 
  - a vector giving the values for each variable. 
  
  \code{c("1,2", "1,2,3")} would mean that the first variable takes values 1 and 2, 
  and the second variable takes values 1, 2 and 3.
  
  - a list giving the values for each variable. 
  \code{list(c(1,2), c(1,2,3))} would mean that the first variable takes values 1 and 2, 
  and the second variable takes values 1, 2 and 3.
  
  - a compact notation using the hash symbol to separate variables
  \code{"1,2#1,2,3"}
  
  Additionally for the \code{probs} argument, a check is performed to make sure that each 
  variable probability sums to 1.
  
  Alternatively, a \code{probArray} argument can be given. This should be a data frame containing 
  one more column (named "prob") than the number of variables to create. Each variable has a column
  which contains the values it can take. The prob column gives the probability for each combination.
  See examples. The prob column should sum up to one.
}
\value{
  A data frame.
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ 
  \code{\link{createContinuousCovariates}}, 
  \code{\link{createExternalCovariates}},  
  \code{\link{createTimeVaryingCovariates}}, and
  \code{\link{createCovariates}}
}
\examples{
  
  # 10 samples of X and Y where: 
  # P[ X = 1 ] = .1
  # P[ X = 2 ] = .9
  # -
  # P[ Y = 7 ] = .5
  # P[ Y = 8 ] = .4
  # P[ Y = 9 ] = .1
  dat <- createDiscreteCovariates( 10 , names = "X, Y", probs = ".1,.9#.5,.4,.1", values = "1,2#7,8,9")
  print(dat)
  stopifnot( all( dat$X \%in\% c(1,2)) )
  stopifnot( all( dat$Y \%in\% c(7,8,9)) )
  
  
  # using the probArray version
  pa <- data.frame( F1 = rep(0:1, 3), F2 = rep(1:3, each = 2), PROB = c(.1,.2,.1,.2,.2,.2) )
  print( pa )
  createDiscreteCovariates( 100 , probArray = pa )
  
}
\keyword{ datagen }

