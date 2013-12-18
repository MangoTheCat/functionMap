
#######################################################################################################

## Set the Environment - this code executes on build to ensure it exists in the MSToolkit library!

# This line sets the initial (empty) environment
.ectdEnv <- new.env( )

# This line sets the default "logging" file to "ectd.log"
assign( "logfile", "ectd.log", envir = .ectdEnv )

# This line sets the default "verbose" behaviour, which determines the amount of logging performed
# This is used, in particular, when errors are generated
assign( "verbose", FALSE, envir = .ectdEnv )

# This line sets the default data format (used for the format of data in the logging)
assign( "dateFormat","%Y-%m-%d %H:%M:%OS4" , envir = .ectdEnv )

# This sets the current data storage method
assign("dataStoreMethod", "CSV", envir = .ectdEnv)

# This sets the default column names
assign("colNames", list(
	Subject = list(Name = "SUBJ", Other = "ID", Default = "SUBJ"),
	Time = list(Name = "TIME", Other = c("DAY", "WEEK"), Default = "TIME"),
	Dose = list(Name = "DOSE", Other = "", Default = "DOSE"),
	Interim = list(Name = "INTERIM", Other = "", Default = "INTERIM"),
	ParOmit = list(Name = "PAROMIT", Other = "", Default = "PAROMIT"),
	RespOmit = list(Name = "RESPOMIT", Other = "", Default = "RESPOMIT"),
	Response = list(Name = "RESP", Other = "DV", Default = "RESP"),
	Trt = list(Name = "TRT", Other = "", Default = "TRT"),
	Missing = list(Name = "MISSING", Other = "", Default = "MISSING"),
	Replicate = list(Name = "Replicate", Other = "TRIAL", Default = "Replicate"),
	DrugName = list(Name = "DRUGNAME", Other = "", Default = "DRUGNAME"),
	Drug = list(Name = "DRUG", Other = "", Default = "DRUG")
), envir = .ectdEnv)

#######################################################################################################

## The rest of this script sets the access functions for the meta layer

# get or set the logfile
getEctdLogFile <- function( ) get("logfile", envir = .ectdEnv)

setEctdLogFile <- function(file) {
	if( missing(file) ) ectdStop("Must provide log file") 
	assign("logfile", file, envir = .ectdEnv )
	invisible(file)
}

# get or set the verbose
getEctdVerbose <- function() get("verbose", envir = .ectdEnv)

setEctdVerbose <- function(verbose) {
	if( missing(verbose) ) ectdStop("Must provide verbose flag")
	assign("verbose", verbose, envir = .ectdEnv )
	invisible(verbose)  
}

# get or set the dateFormat
getEctdDateFormat <- function( ) get("dateFormat", envir = .ectdEnv)

setEctdDateFormat <- function(format){
	if( missing(format) ) ectdStop("Date format must be provided")
	assign("dateFormat", format, envir = .ectdEnv )
	invisible(format)     
}

# Get or set the data storage method
getEctdDataMethod <- function() get("dataStoreMethod", envir = .ectdEnv)

setEctdDataMethod <- function(method) {
	if( missing(method) ) ectdStop("Must provide a data storage method: 'CSV', 'RData' or 'Internal'")
	method <- match.arg(method, c("CSV", "RData", "Internal"))
	assign("dataStoreMethod", method, envir = .ectdEnv )
	invisible(method)
}

# Get & Set external execution path
getEctdExternalPath <- function( pathName ) {
	getPaths <- get("externalPaths", envir = .ectdEnv)
	if (missing(pathName)) {
		return(names(getPaths))	
	}
	else {
		if (!is.character(pathName) || length(pathName) != 1) ectdStop("Single character value should be provided as the 'pathName' input")
		if (pathName %in% names(getPaths)) return(getPaths[pathName]) 
		else ectdStop(paste("Could not find external path '", pathName, "'", sep=""))
	}
} 

setEctdExternalPath <- function(pathName, Value) {
	if (missing(pathName) || !is.character(pathName) || length(pathName) != 1) ectdStop("Single character value should be provided as the 'pathName' input")
	if (missing(Value) || !is.character(Value) || length(Value) != 1) ectdStop("Single character value should be provided as the 'Value' input")
	getPaths <- get("externalPaths", envir = .ectdEnv)
	getPaths <- getPaths [ setdiff(names(getPaths), pathName) ]
	getPaths <- c(getPaths, Value)
	names(getPaths)[length(getPaths)] <- pathName
	assign("externalPaths", getPaths, envir = .ectdEnv)
	invisible(getPaths)
}

# Get, set and reset default column names
getEctdColName <- function(colName) {
	if (missing(colName) || !is.character(colName) || length(colName) != 1) ectdStop("Single character value should be provided as the 'colName' input")
	getNames <- get("colNames", envir = .ectdEnv)
	if (colName %in% names(getNames)) return(getNames[[colName]]$Name)
	else ectdStop(paste("Provided column name '", colName, "' cannot be found", sep=""))
}

setEctdColName <- function(colName, Value) {
	if (missing(colName) || !is.character(colName) || length(colName) != 1) ectdStop("Single character value should be provided as the 'colName' input")
	if (missing(Value) || !is.character(Value) || length(Value) != 1) ectdStop("Single character value should be provided as the 'Value' input")
	getNames <- get("colNames", envir = .ectdEnv)
	if (colName %in% names(getNames)) getNames[[colName]]$Name <- Value
	else ectdStop(paste("Provided column name '", colName, "' not a valid column to set", sep=""))
	assign("colNames", getNames, envir = .ectdEnv)
	invisible(getNames)
}

resetEctdColNames <- function( whichNames = names(getNames)) {
	getNames <- get("colNames", envir = .ectdEnv)
	if (!is.character(whichNames)) ectdStop("Character vector should be provided as the 'whichNames' input")
	whichNames <- whichNames [ whichNames %in% names(getNames)]
	if (length(whichNames)) {
		for (i in whichNames) getNames[[i]]$Name <- getNames[[i]]$Default 
	}
	assign("colNames", getNames, envir = .ectdEnv)
	invisible(getNames)
}

getEctdPossibleColNames <- function(colName) {
	if (missing(colName) || !is.character(colName) || length(colName) != 1) ectdStop("Single character value should be provided as the 'colName' input")
	getNames <- get("colNames", envir = .ectdEnv)
	if (colName %in% names(getNames)) {
		outVec <- unique(c(getNames[[colName]]$Name, getNames[[colName]]$Other))
		return(setdiff(unique(outVec), ""))
	}
	else ectdStop(paste("Provided column '", colName, "' cannot be found", sep=""))
}

setEctdPossibleColNames <- function(colName, Value) {
	if (missing(colName) || !is.character(colName) || length(colName) != 1) ectdStop("Single character value should be provided as the 'colName' input")
	if (missing(Value) || !is.character(Value) || !length(Value)) ectdStop("A character vector of possible names must be provided")
	getNames <- get("colNames", envir = .ectdEnv)
	if (colName %in% names(getNames)) {
		getNames[[colName]]$Other <- unique(Value)
		assign("colNames", getNames, envir = .ectdEnv)
		invisible(getNames)
	}
	else ectdStop(paste("Provided column '", colName, "' cannot be found", sep=""))
}

matchEctdColNames <- function(colName, dataNames) {
	colList <- getEctdPossibleColNames(colName)
	if (!any(myTest <- colList %in% dataNames)) return(NULL)
	else colList[myTest][1]
}

