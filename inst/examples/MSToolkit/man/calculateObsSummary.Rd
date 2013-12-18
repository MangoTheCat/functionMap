\name{calculateObsSummary}
\alias{calculateObsSummary}
\title{Calculate Observed Data Summary}
\description{
Calculate a summary of an "observed" dataset (typically used to overlay observed data in a simulated plot or table)
}
\usage{
calculateObsSummary(data, respCol = getEctdColName("Response"), bVar = NULL, subset = NULL, 
	alpha = 95, digits = 3, diffBase = FALSE, doseCol = getEctdColName("Dose"), timeCol = getEctdColName("Time"), 
	idCol = getEctdColName("Subject"), respType = c("Continuous", "Categorical"), 
	catType = c("Count", "Proportion"), fillRespRange = TRUE) 
}
\arguments{
        \item{data}{ Observed data frame } 
        \item{respCol}{ Response column names (given by \link{getEctdColName} by default)} 
        \item{bVar}{ Variables by which summary should be produced (none by default) } 
        \item{subset}{ Subsets to be applied to the observed data before calculating the summary } 
        \item{alpha}{ Alpha value for calculation of lower and upper intervals  } 
        \item{digits}{ Number of digits to round summary data } 
        \item{diffBase}{ Logical: Should differences from baseline be summarised instead of raw data? } 
        \item{doseCol}{ Dose column names (given by \link{getEctdColName} by default) } 
        \item{timeCol}{ Time column names (given by \link{getEctdColName} by default) } 
        \item{idCol}{ Subject column names (given by \link{getEctdColName} by default) } 
        \item{respType}{ Response type: Continuous (default) or Categorical } 
        \item{catType}{ For Categorical response, should "Count" (default) or "Proportion" summary be returned? } 
        \item{fillRespRange}{ For Categorical response, should we "fill" the range of responses (see below) }
}
\details{
	The inputs are checked, and the alpha value is parsed using \link{checkSimAlpha}
	Subsets are applied to the data if "subset" is specified and differences from baseline are calculated using \link{calculateDiffsFromBase} if required

	If respType is "Continuous":
	* Mean responses are calculated by Subject (\code{idCol}), Dose (\code{doseCol}), Time (\code{timeCol}), and any by variables ((\code{bVar})
	* The following summaries are then created (based on any by variables (\code{bVar}))
		- Mean
		- Median
		- Minimum
		- Maximum
		- Number of non-missing values
		- Lower alpha\% percent interval
		- Upper alpha\% percent interval

	When dealing with categorical responses, it is possible that the unique set of responses could be (say): 1, 2, 4, 5
	In this case, it is unclear as to whether the value "3" should be included in a summary.  If "fillRespRange" is TRUE, it would be included (although would have a count/proportion of zero)
	Frequencies are calculated by "Response level" (\code{respCol}) and any by variables
	Frequences are converted to proportions (within by variable level) if required (\code{catType})
}
\value{
	A data frame of response summaries
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\keyword{ Observed }
