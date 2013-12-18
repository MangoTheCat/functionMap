###############################################################################
# Mango Solutions, Chippenham SN14 0SQ 2006
# utils.R Fri Jun 01 10:57:12 BST 2007 @456 /Internet Time/
#
# Author: Romain
###############################################################################
# DESCRIPTION: set of utility functions, not exported
# KEYWORDS: component:helper 
###############################################################################

.nonCumulativeFromCumulative <- function( proportion ){
  
  ## format it correctly
  proportion <- parseCharInput( proportion ) 
  
  ## check that the numbers are probabilities \in (0,1]
  if(max(proportion) >  1) ectdStop("Proportion greater than 1")
  if(min(proportion) <= 0) ectdStop("Proportion lower than 0")
  
  ## append 1 if needed
  if(max(proportion) != 1) proportion <- c(proportion, 1)
  
  ## generate the non-cumulative proportions
  proportion <- diff( c(0, proportion)  )
  if(any(proportion <= 0)) ectdStop("Proportion not increasing")
  proportion
}

.deriveFromMasterSeed <- function(){
	sample(1:999, 1) 
}
 
### regular expression toys
                                
.quotes <- "(\\\"|\')"
        

.strinterp <- function(txt){
  myrx <- "\\$(\\{(.*?)\\}|[\\.]?[a-zA-Z][\\.0-9a-zA-Z]*)"
  
  gx  <- gregexpr( myrx , txt, perl = TRUE)[[1]]
  va  <- substring( txt, gx + 1, gx + attr(gx, "match.length")-1) %-~% "[\\{\\}]"
  spl <- strsplit( txt, myrx , perl = TRUE )[[1]]
  if(length(va) == 1 && va == "") return(txt)
  if(length(va) == length(spl)) spl <- c(spl, "")

  out <- spl[1]
  for( i in seq( along = va )) {
    varName <- try( get(va[i], pos = parent.frame(2) )[1] ,silent = TRUE )
    if( class(varName) == "try-error") varName <- va[i]
    out <- out %.% varName  %.% spl[i+1]
  }
  out
}

.allSameLength <- function( ... ){
  all( diff( sapply(list(...), length)) == 0 ) 
}

.requiredArgs <- function(arg, msg){
  if( missing(arg) ) {
    sc <- sys.call()
    nameArg <- as.character(sys.call()[2])
    if( missing(msg) ) msg <- "The argument `" %.% nameArg %.% "` is missing" 
    ectdStop(msg)
  }
}

# changes 10 into 1:10, check that 10 is positive
# write the number of subjects in the parent environment as `nSubjects`
.expandSubjects <- function(subjects){
  if(length(subjects) == 1) {
    if(subjects < 1) ectdStop("subjects must be positive")
    subjects <- 1:subjects 
  } else {
    subjects
  }
  assign("nSubjects", length(subjects), parent.frame())
  subjects
}

.handleProbArray <- function( probArray, values, probs) {
	if( !missing(probArray) && is.matrix(probArray) ) {
		sum(probArray) == 1 || ectdStop("`probArray` does not sum up to one")
		if( missing(values) ) {
			values <- dimnames(probArray)
			if( is.null(values) ) {
				values <- sapply( dim(probArray), seq)
			}
		}
		if( !all(  dim(probArray)  == sapply(values, length)  )) ectdStop("Dimension problem between `probArray` and `values`")
		out <- cbind( do.call( expand.grid, values ), probs = as.vector( probArray) )
		out <- subset( out, probs > 0)
		
	} else { 
		## make tests on the probArray or build it
    	if ( missing(probArray) ) {
			# try to build it
			if(missing(values) || missing(probs) ) ectdStop("`values` and `probs` must be supplied if probArray is missing")
			probArray <- probs[[1]]
			lp <- length(probs)
			if(lp>1) {
				for(idx in 2:lp) {
					probArray <- probArray %o% probs[[idx]]
				}
			}
		} else {
			# check the probArray
			if (is.data.frame(probArray)) {
				if(sum(probArray[,ncol(probArray)]) != 1) ectdStop("the probArray does not sum up to one")
				if(ncol(probArray) - 1 != length(values)) ectdStop("The `probArray` has wrong dimensions")
			}
			
			if (is.array(probArray)) {
				if(sum(probArray) != 1) ectdStop("the probArray does not sum up to one")
				if(length(dim(probArray)) != length(values)) ectdStop("The `probArray` has wrong dimensions")
			}
			
		}
    
		## make the grid                               
		out <- cbind( do.call( expand.grid, values ), probs = as.vector(probArray) )
	}
	## make sure they are all factors
	for( va in 1:length(values)) {
		out[,va] <- factor(out[,va])
	}
	out
}

.eval <- function( txt ){
  eval( parse( text = .strinterp(txt) ), parent.frame() )
}

.dummy <- function(...){
  cat( "\n\nFunction `"%.% as.character(match.call()[1]) %.% "` not yet implemented\n\n" ) 
}

.log <- function( ... , file = getEctdLogFile(), verbose = getEctdVerbose()){
  if( verbose ){ 
    msg <- paste( ..., "\n", sep = "" )
    msg <- .strinterp( msg )
    msg <- sprintf( "[%s] %s", format( Sys.time(), getEctdDateFormat() ) , msg )
    cat( msg, file = file, append = TRUE)
  }
  invisible( NULL )
}

".checkFun" <- function(
	fun, 
	expectedArgs
){
	fun <- try( eval( match.fun(fun), parent.frame() ), silent = TRUE )
	if (class(fun) == "try-error") ectdStop("not a valid function")   
	if( !missing(expectedArgs) ){  
		expectedArgs <- parseCharInput( expectedArgs, convertToNumeric = FALSE )           
		if( !all(expectedArgs %in% names(formals(fun))) ) 
			ectdStop("Problem with the arguments of the function")
	}
	fun
}
".checkReplicates" <- function( replicates, workingPath = getwd(), method = getEctdDataMethod() )
{
	## Ensure the replicate files exist
	repsExist <- try(getReplicates(workingPath = workingPath, method = method), silent = TRUE)
	if (class(repsExist) == "try-error") ectdStop("No replicates found")
	if (is.character(replicates)) {
		if (length(replicates) == 1 && replicates == "*") return(repsExist)
		else ectdStop("'replicates' input in the wrong format")
	}
	else {
		if (is.numeric(replicates)) {
			if (!all(replicates %in% repsExist)) ectdStop("Could not find all replicates specified")
			else return(replicates)
		}
		else ectdStop("'replicates' input must be a numeric vector or a single character ('*')")
	}
	NULL
}

.checkGridAvailable <- function(){
  suppressWarnings(require(parallel, quietly = TRUE))
}

.splitGridVector <- function(vec, nReps = 100) {
  startVec <- rep(1:ceiling(length(vec)/nReps), each=nReps)[1:length(vec)]
  if (any(startVec > 1) & sum(startVec == max(startVec)) < .1 * nReps) {
    startVec[startVec == max(startVec)] <- max(startVec)-1
  }
  split(vec, startVec)
}


.ectdSasCall <- function(params, 
  sasLoc = if (.Platform$OS.type == "windows") getEctdExternalPath("SASPATH_WIN") else getEctdExternalPath("SASPATH_UNIX"), 
  macroLoc = file.path(path.package("MSToolkit"), "sasAnalysis.sas"), 
  logFile = file.path(workingPath, "sasLogfile.log"),
  printFile = file.path(workingPath, "sasOutput.lst"),
  workingPath = getwd()) 
{
	sasDir <- gsub( "\\\\[^\\]*$", "", sasLoc)
	if(!file.exists(sasDir)) ectdStop("SAS is not available on the system: \n\t$sasLoc")
	callOptions <- if (.Platform$OS.type == "windows") "-nosplash -icon -xmin -noxwait" else "-NOTERMINAL"
	sasCall <- paste("\"", sasLoc, "\" -SYSIN \"", macroLoc, "\" -SYSPARM \"", params, "\" -LOG \"", logFile, "\" -PRINT \"", printFile, "\" ", callOptions, sep="")
	.log("Calling SAS with call string: ", sasCall)
	invisible(try(system(sasCall)))
}


.roundIt <- function( data, digits ){
  if(!missing(digits)){   
    digits <- parseCharInput( digits, convertToNumeric = TRUE )
    nCov <- ncol( data )
    if(any(digits < 0 )) ectdStop("`digits` should be a positive vector")
    len.di <- length(digits)
    if( len.di == 1 ) {
      data <- round( data, digits = digits)
    } else if( len.di == nCov ) {
      for( i in 1:nCov){
        data[,i] <- round( data[,i], digits = digits[i] )
      }
    } else {
      ectdStop("`digits should be of length one or $nCov, not ${len.di}`")
    }
  }
  data
}


.cleanup <- function( 
  cleanUp = TRUE,       # do I do any cleanup
  grid = FALSE,         # was the grid used
  workingPath = getwd(), # where to work 
  method = getEctdDataMethod()  # Method of data storage to "clean"
  ){
  
	if( cleanUp ){
		if (method %in% c("CSV", "RData")) {
			.log("removing micro and macro directories")
			removeDirectories(c("MicroEvaluation", "MacroEvaluation"), workingPath = workingPath)
			if(grid){ 
				allconns <- showConnections()
				sockconns <- rownames(allconns)[which(allconns[, "class"] == "sockconn")]
				if (length(sockconns) > 0) for (i in sockconns) try(close(getConnection(i)))
				.log("clusters were shut down")
			}
		}
		else {
			ectdStop("Have not yet implemented the cleanup of 'Internal' data stored")
		}
	}
  
}

.applyDataSubset <- function(df, subset) {
	if (!is.data.frame(df)) ectdStop("Cannot subset a non-data-frame object")
	dataFlags <- try(eval(parse(text = subset), envir = df))
	if (is.logical(dataFlags) && length(dataFlags) == nrow(df)) {
		if (!any(dataFlags)) ectdStop("No rows left following subset")
		return(df[dataFlags,,drop = FALSE])
	}
	else ectdStop("Could not apply subset to the data")
}

# A replacement for aggregate.data.frame
.dataAggregate <- function(resp, by, fun, ..., bound = 10^5) {
	if (length(by) == 1) return(aggregate(resp, by, fun, ...))
	uniCols <- sapply(by, function(x) length(unique(x)))
	byProd <- prod(uniCols)
	if (byProd > bound) {
		whichSplit <- which(uniCols == max(uniCols))[1]
		allData <- data.frame(resp, by)
		splitData <- split(allData, by[[whichSplit]])
		splitAgg <- lapply(splitData, function(df, respCol, byCol, fun, ...) {
					aggregate(df[respCol], df[byCol], fun, ...)
				}, respCol = names(resp), byCol = names(by), fun = fun, ...)
		outDf <- do.call("rbind", splitAgg)
		theOrder <- do.call("order", outDf[rev(names(by))])
		outDf <- outDf[theOrder,,drop = FALSE]
		row.names(outDf) <- 1:nrow(outDf)
		outDf
	}
	else return(aggregate(resp, by, fun, ...))
}

# Image Comparison Function
.compareImages <- function(result, target, 
		exeLoc = system.file(package = "MSToolkit", "ImageCompare", "diff.exe")) {
	if (!file.exists(result) | !file.exists(target)) return(FALSE)
	buildCall <- paste(exeLoc, shortPathName(result), shortPathName(target))
	x <- system(buildCall, intern = TRUE, wait = TRUE)
	!(length(x) && length(grep(" differ", x)))
}
