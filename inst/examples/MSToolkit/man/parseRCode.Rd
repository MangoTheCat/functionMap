\name{parseRCode}
\alias{parseRCode}
\title{ Parse R Code }
\description{
Parses character string to ensure it is valid R code, returning an R "expression" object
}                    
\usage{
parseRCode(code)
}
\arguments{
\item{code}{Character string containing R Code to parse}
}
\details{
The \code{parseRCode} function wraps "parse" and checks to ensure a valid expression is created
}
\value{
An \link{expression} object
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }

