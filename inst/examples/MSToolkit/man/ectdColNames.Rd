\name{getEctdColName}
\alias{getEctdColName}
\alias{setEctdColName}
\alias{resetEctdColNames}
\alias{matchEctdColNames}
\alias{getEctdPossibleColNames}
\alias{setEctdPossibleColNames}
\title{ Control of default column names }
\description{
Functions that allow control over the default column names for simulated data
}
\usage{
getEctdColName(colName) 
setEctdColName(colName, Value) 
getEctdPossibleColNames(colName) 
setEctdPossibleColNames(colName, Value) 
resetEctdColNames(whichNames = names(getNames)) 
matchEctdColNames(colName, dataNames)
}
\arguments{
\item{colName}{ (Required) The "column type" of the variable name of interest (one of 'Subject', 'Time', 'Dose', 'Interim', 'ParOmit', 'RespOmit', 'Response', 'Trt', 'Missing', 'Replicate', 'DrugName' and 'Drug') }
\item{Value}{ (Required) Value to which to set the default column name }
\item{dataNames}{ (Required) Column names against which to match the possible set of column names }
\item{whichNames}{ Column types for which to reset the default name (default all) }
}
\details{

	The functions provide the following capabilities:
	* getEctdColName - Gets the current default column name given a column type
	* setEctdColName - Sets the current default column name for a column type
	* resetEctdColNames - Resets the current default column names to their initial state
	* getEctdPossibleColNames - Gets the set of possible column names given a column type
	* setEctdPossibleColNames - Sets the set of possible column names given a column type
	* matchEctdColNames - Selects a column name from a set of names that best matches the possible column type names

	The set of possible "column types" are:
	* Subject - Subject column
	* Dose - Dose column
	* Time - Time column
	* Replicate - Replicate column names
    * Interim - Interim allocation column name
    * ParOmit - Parameter "omit" flag column name
    * RespOmit - Response "omit" flag column name
    * Response - Response column name
    * Trt - Treatment column name
    * Missing - Parameter "omit" flag column name
    * DrugName - Name of column containing the "Drug Name" (used in typical value simulations)
    * Drug - Name of column containing the "Drug Value" (used in typical value simulations)

}
\value{

The "getEctdColName" function returns a single character, giving the current column name

The "getEctdPossibleColNames" function returns a character vector, giving a set of possible columns

The "matchEctdColNames" function returns a single character identifying the variable in "dataNames" that should be used as the "colName" column

The other functions to not explicitly return anything

}
\author{ Mike K Smith \email{mstoolkit@googlemail.com} }
\examples{\dontrun{

	getEctdColName("Subject")								# Get the current Subject variable
	setEctdColName("Subject", "ID")							# Set the current Subject variable
	getEctdColName("Subject")								# Get the "new" current Subject variable
	resetEctdColNames()										# Reset all current column name defaults
	getEctdPossibleColNames("Subject")						# Get set of possible "Subject" variable names
	matchEctdColNames ("Subject", c("A", "SUBJ", "B"))		# Finds the "Subject" column in a set of possible names

}}
\keyword{ IO }
