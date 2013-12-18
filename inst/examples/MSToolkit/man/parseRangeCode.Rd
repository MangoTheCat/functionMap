\name{parseRangeCode}
\alias{parseRangeCode}

\title{Parse a numerical range expression} 
\description{parseRangeCode: Converts inequalities of up to two comparators stored in a vector of strings into an executable R expression.  For
 instance, parseRangeCode(c("1 < Y < 20", "40 < Y")) will yield the expression (1<Y)&(Y<20)&(40<Y).
 parseRCode: simply parse an R string into an expression.
}

\usage{
parseRangeCode(code)
}

\arguments{
  \item{code}{ (Required) For parseRangeCode: A vector of strings that contain inequalities along with various seperators (see below).  Each inequality should have either 1 or 2 comparators. For parseRCode: a code string. For \code{parseRCode} a character vector containing code. }
}
\details{
  The \code{parseRangeCode} function converts various kinds of ranges/inequalities that are normally not handled by R into R-executable
expressions.  For instance it will convert inequalities of the form "A < B < C" into (A < B) & (B < C), where < may be replaced with 
any comparator.  Moreover it allows inqualities to be concatenated by the symbols "&", ";" or "," which are all treated as equivalent
to the logical "and" (i.e. "&").  Thus "A < B <C & D > E, F <= G" will be converted into (A < B)&(B < C)&(D > E)&(F <= G). In 
addition if \code{code} is a vector with more than one element, each element will be parsed and then concatenated into a
single expression with "&".  Hence c("1 < Y", "2 < Z <= 5") would become \code{(1 < Y)&(2 < Z)&(Z <= 5)}.
}
\value{
An expression constructed as detailed above.
}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\section{Warning}{  The character "|" is not allowed in any of the expressions contained in "code" for the \code{parseRangeCode} function.}
\seealso{ \code{\link{subset}}, \code{\link{parse}}  }
\examples{
# Examples of using subsets
exData <- data.frame( Y = rnorm(200), B = rnorm( 200 ) )
subs1 <- parseRangeCode("1 < Y < 10 & 1 > B > -2")
exData[ eval(subs1, exData), ]

subs2 <- parseRangeCode(c("1 < Y < 10", "1 > B > -2"))
exData[ eval(subs1, exData), ]

expr <- parseRCode("rnorm(30)")
eval( expr ) 
}
\keyword{misc}

