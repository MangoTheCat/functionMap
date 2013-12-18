\name{checkSimAlpha}
\alias{checkSimAlpha}
\title{ Parse alpha value }
\description{
	Parses an input alpha value to be used in numeric summary calculations
}                    
\usage{
checkSimAlpha(alpha = 95)
}
\arguments{
\item{alpha}{ Alpha value to be parsed }
}
\details{
The function first checks that there is only 1 input.  If character, the alpha is parsed and converted to a single numeric.
If alpha is greater than 1, it is assumed to be on the [1,100] scale, and will be scaled onto the [0,1] scale
If alpha is less than 0.5, it is assumed to tbe on the [0,.5] scale (ie. .05 instead of .95) and converted to the [.5, 1] scale
}
\value{
	A single numeric between .5 and 1
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{
	checkSimAlpha("95\%")
	checkSimAlpha(95)
	checkSimAlpha(5)
}

\keyword{ statistics }
