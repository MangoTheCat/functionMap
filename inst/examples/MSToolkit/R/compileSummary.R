"compileSummary" <- function (
		dataType = c("MicroEvaluation", "MacroEvaluation"), 
		replicates = NULL, 
		prefix = switch(dataType, MicroEvaluation = "micro", MacroEvaluation = "macro"),
		replicateCol = getEctdColName("Replicate"),
		workingPath = getwd()
)
{
	# Check the inputs
	dataType <- try(match.arg(dataType), silent = TRUE)
	if (class(dataType) == "try-error") ectdStop("Invalid data type: $dataType")
	prefix <- casefold(substring(dataType, 1, 5))
	if (!file.exists(file.path(workingPath, dataType))) ectdStop("directory $dataType unavailable under $workingPath")
	if (!length(repfiles <- dir(file.path(workingPath, dataType), pattern = "m[ia]cro[[:digit:]]{4}\\.csv"))) ectdStop("no data files under $workingPath/$dataType")
	if (!is.null(replicates) && !all(out <- file.exists(targetfiles <- file.path(workingPath, dataType, sprintf("%s%04d.csv", prefix, replicates))))) 
		ectdStop(paste("Impossible to compile the data, the following files are missing: ", paste(targetfiles[!out], collapse = "\n\t"), sep = "\n\t"))

	# Work out replicates to compile
	nRep <- if (is.null(replicates)) length(repfiles) else length(replicates)
	.log("Compiling $nRep $dataType files")
	if (!length(replicates)) replicates <- getReplicates(dataType, prefix, "CSV", workingPath = workingPath)

	# Read all text from datasets
	allPaths <- sapply(replicates, .dataGetFullPath, dataType = dataType, workingPath = workingPath, method = "CSV") 
	getText <- lapply(1:length(allPaths), function(i, paths, colName ) {
				getData <- readLines(paths[i])
				paste(c(colName, rep(i, length(getData)-1)), getData, sep=",")
			}, paths = allPaths, colName = paste("\"", replicateCol, "\"", sep=""))

	# Check columns
	firstRows <- sapply(getText, "[[", 1)
	if (!all(firstRows == firstRows[1])) ectdStop("All analysis outputs must have same column names")

	# Write to external file	
	outText <- c(getText[[1]][1], do.call("c", lapply(getText, "[", -1)))
	summaryFile <- file.path(workingPath, sprintf("%sSummary.csv", prefix ))
	cat(outText, file = summaryFile, sep="\n")
}
