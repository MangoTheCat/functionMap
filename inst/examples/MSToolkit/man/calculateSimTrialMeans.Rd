\name{calculateSimTrialMeans}
\alias{calculateSimTrialMeans}
\title{Calculate Simulated Trial Means}
\description{
	Calculate mean simulated responses by replicate
}
\usage{
calculateSimTrialMeans(data, respCol = getEctdColName("Response"), bVar = doseCol, subset = NULL, 
	diffBase = FALSE, idCol = getEctdColName("Subject"), timeCol = getEctdColName("Time"), 
	doseCol = getEctdColName("Dose"), replicateCol = getEctdColName("Replicate"), 
	respType = c("Continuous", "Categorical"), catType = c("Proportion", "Count"), 
	fillRespRange = TRUE, digits = 3) 
}
\arguments{
\item{data}{ Simulated trial data frame } 
\item{respCol}{ Response column names (given by \link{getEctdColName} by default)} 
\item{bVar}{ Variables by which means should be produced ("Dose" by default) } 
\item{subset}{ Subsets to be applied to the data before calculating the means } 
\item{diffBase}{ Logical: Should differences from baseline be summarised instead of raw data? } 
\item{idCol}{ Subject column names (given by \link{getEctdColName} by default) } 
\item{timeCol}{ Time column names (given by \link{getEctdColName} by default) } 
\item{doseCol}{ Dose column names (given by \link{getEctdColName} by default) } 
\item{replicateCol}{ Replicate column names (given by \link{getEctdColName} by default) } 
\item{respType}{ Response type: Continuous (default) or Categorical } 
\item{catType}{ For Categorical response, should "Count" (default) or "Proportion" summary be returned? } 
\item{fillRespRange}{ For Categorical response, should we "fill" the range of responses (see below) }
\item{digits}{ Number of digits to round summary data } 
}
\details{
	The inputs are checked, and the alpha value is parsed using \link{checkSimAlpha}
	Subsets are applied to the data if "subset" is specified and differences from baseline are calculated using \link{calculateDiffsFromBase} if required

	If respType is "Continuous":
	* Mean responses are calculated by Replicate (\code{replicateCol}), Subject (\code{idCol}), Dose (\code{doseCol}), Time (\code{timeCol}), and any by variables (\code{bVar})
	* Means of these means are then calculated by Replicate (\code{replicateCol}) and any by variables (\code{bVar})
	* The data is rounded (\code{digits}) and returned
	
	When dealing with categorical responses, it is possible that the unique set of responses could be (say): 1, 2, 4, 5
	In this case, it is unclear as to whether the value "3" should be included in a summary.  If "fillRespRange" is TRUE, it would be included (although would have a count/proportion of zero)
	Frequencies are calculated by "Response level" (\code{respCol}) and any by variables
	Frequences are converted to proportions (within by variable level) if required (\code{catType})

}
\value{
	A data frame of trial "means"
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
