"calculateDiffsFromBase" <- function(
		data, 										#@ Data (frame) input
		respCol = getEctdColName("Response"), 		#@ Response variable name
		idCol = getEctdColName("Subject"),			#@ Subject variable name
		timeCol = getEctdColName("Time"), 			#@ Time variable name 
		replicateCol = getEctdColName("Replicate"),	#@ Replicate variable name
		baseDef = paste(timeCol, "<= 0"),			#@ Baseline definition
		tolerance = 1e-005,							#@ Number below which there difference is assumed to be 0
		removeBaseline = TRUE						#@ Remove the baseline data before returning
)
{
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# calculateDiffsFromBase.R 10NOV09
	# 
	# Author: Rich
	###############################################################################
	# DESCRIPTION: Calculate differences from baseline for a response variable
	###############################################################################
	# Basic input checks
	.checkLogical(removeBaseline)										# Check single logicals
	.checkCharacter(respCol, idCol, timeCol, baseDef)		# Check single characters
	.checkNumeric(tolerance)											# Check single numerics
	if (!length(replicateCol)) replicateCol <- c()
	
	# Inner function: warn about not calcualting diffs from baseline
	innerBaseWarn <- function(str) ectdWarning(paste(str, "will not calculate differences from baseline", sep=" - "))
	
	# Check input data
	if (!is.data.frame(data)) {
		innerBaseWarn("First input to calcualteDiffsFromBase must be a data frame")
		return(data)
	}

	# Check required variables
	neededCols <- c(respCol, idCol, timeCol)
	if (!all(neededCols %in% names(data))) {
		innerBaseWarn("Couldn't find all requried variables")
		return(data)
	}
	
	# Apply "baseline" definition to extract 
	allIds <- unique(data[c(replicateCol, idCol)])			# Unique subjects
	isBase <- try(with(data, eval(parse(text = baseDef))))
	if (!is.logical(isBase)) {
		innerBaseWarn("Problem when defining baseline values")
		return(data)
	}
	
	# Test availability of baseline data
	if(!any(isBase)) {
		innerBaseWarn("No baseline data found")		# Fail and return if no baseline data
		return(data)
	}
	if(all(isBase)) {
		innerBaseWarn("All data considered to be baseline data")		# Fail and return if no baseline data
		return(data)
	}
	baseData <- data[isBase,  , drop = FALSE] 					# Select baseline data
	uniBaseId <- unique(baseData[c(replicateCol, idCol)])		# Select subjects in baseline data
	gotAllBase <- nrow(allIds) == nrow(uniBaseId)				# Test if all subjects have baseline data
	if (!gotAllBase) {
		innerBaseWarn("Not all subjects have baseline data")		# Fail and return if no baseline data
		return(data)
	}
	
	data$.origOrder <- 1:nrow(data)			# Save original data order
	
	# Calculate mean baseline data by ID
	baseData <- .dataAggregate(baseData[, respCol, drop = FALSE], baseData[, c(replicateCol, idCol, timeCol), drop = FALSE], mean, na.rm = TRUE)	# Mean baseline response by Replicate, Subject and Time	
	baseData <- .dataAggregate(baseData[, respCol, drop = FALSE], baseData[, c(replicateCol, idCol), drop = FALSE], mean, na.rm = TRUE)				# Mean of those means by Replicate and ID
	names(baseData)[names(baseData) == respCol] <- ".baseLine"				# Rename "respCol" to ".baseLine"
	
	# Merge data and subtract baseline for each subject
	mergeData <- merge(data, baseData)								# Merge baseline data
	mergeData[[respCol]] <- mergeData[[respCol]] - mergeData$.baseLine		# Calculate differences from baseline
	mergeData[[respCol]][abs(mergeData[[respCol]]) < tolerance] <- 0			# Replace very small differences with 0
	
	# Reorder to exclude baseline data
	retData <- mergeData [ order(mergeData$.origOrder), setdiff(names(mergeData), c(".baseLine", ".origOrder"))]
	if (removeBaseline) retData <- retData[!isBase,,drop=FALSE]
	retData
}
