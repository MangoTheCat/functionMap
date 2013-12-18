\name{allocateTreatments}
\alias{allocateTreatments}
\title{ Allocate treatment to subjects }
\description{
  The \emph{Allocate} component is responsible for allocating simulated 
  subjects to a treatment group. 
}                    
\usage{
allocateTreatments(trts, subjects, prop = NULL, ordered = FALSE, 
  seed = .deriveFromMasterSeed(), idCol = getEctdColName("Subject"), trtCol = getEctdColName("Trt"))

}
\arguments{
 \item{trts}{ (Required) Maximum number of treatments to which subjects can be allocated.  Subjects will be allocated to treatments "1:trts" }
 \item{subjects}{ (Required) Number of subjects to be allocated to each treatment group }
 \item{prop}{ (Optional) Proportion of subjects in each group.  By default, equal proportions of subjects are assigned to each group }
 \item{ordered}{ (Optional) Should treatments be allocated in order of subject number (ie. first N subjects gets treatment 1) as opposed to random assignment.  Default is FALSE (random assignment) }
 \item{seed}{ (Optional) Random seed to allocate interims.  Based on the current random seed by default }
 \item{idCol}{ (Optional) Subject variable name.  "SUBJ" by default }
 \item{trtCol}{ (Optional) Treatment variable name.  "TRT" by default }
}
\details{
 Subjects numbered "1" to "sum(subjects)" are allocated to a single treatment.  The possible treatments are numbered "1" to "trts".

 If "subjects" is a single number, the proportion argument is used to determine the proportion to be allocated to each treatment.  By default, subjects are allocated to each treatment group with probability "1/trts".
 
 If "subjects" is a vector with length "trts", this explicity defines the number of subjects to be allocate to each treatment group.
 
 If "ordered" is TRUE, the allocation is done in order (eg. subject 1 gets treatment 1).  If "FALSE", random allocation is performed.
 
}
\value{
  A data frame with subjects and treatment allocations.  This data frame will contain 2 variables:
\code{SUBJ}{The Subjects identifier }
\code{TRT}{The Treatment numeric that the subject is allocated to }
  ...
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\seealso{ 
  \code{\link{createTreatments}} to create the set of possible treatments given a 
  sequence matrix for a crossover design or a set of doses for a parallel design.
  
  \code{\link{generateData}} that wraps this function.
}
\examples{
  # allocate 6 subsjects to 3 treatment groups in order
  # subjects 1 and 2 will be in treatment group 1, ...
  allocateTreatments(trts = 3, subjects = 6, ordered = TRUE)
  
  # allocate 2 subjects to group 1, 2 to group 2, 3 to group 3
  allocateTreatments(trts = 3, subjects = c(2, 2, 3))
  
  # allocation according to proportions
  # 6 subjects to allocate in total in 2 groups
  # 20% will be in group 1, 80% will be in group 2
  allocateTreatments(trts = 2, subjects = 6, prop = c(0.2, 0.8))
}

\keyword{ datagen }
