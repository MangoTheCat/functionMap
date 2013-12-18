\name{summarizeTrialMeans}
\alias{summarizeTrialMeans}
\title{Summarize Trial Mean Data}
\description{
	Summarize simulated trial mean responses
}
\usage{
summarizeTrialMeans(data, respCol = getEctdColName("Response"), 
	bVar = getEctdColName("Dose"), alpha = 95, digits = 3, method = "Quantile") 
}
\arguments{
        \item{data}{ Simulated data to summarize (data frame) } 
        \item{respCol}{ Response column names (given by \link{getEctdColName} by default)} 
        \item{bVar}{ Variables by which summary should be produced ("Dose" by default) } 
        \item{alpha}{ Alpha value for calculation of lower and upper intervals (default 95) } 
        \item{digits}{ Number of digits to round summary data } 
        \item{method}{ Method to use when calculating lower/upper intervals: Quantile or Gaussian }
}
\details{
	The input data is checked, and the "alpha" value is parsed using \link{checkSimAlpha}
	Response means, medians, minima, maxima and "number of non-missing values" are calculated by any "by" variables (\code{bVar})
	Lower and upper alpha\% intervals are calculated using either the "Gaussian" or "Quantile" method
}
\value{
	A data frame containing the response summaries
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
