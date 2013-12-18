"calculateSimTrialMeans" <- function(
		data, 
		respCol = getEctdColName("Response"), 
		bVar = doseCol,
		subset = NULL, 
		diffBase = FALSE, 
		idCol = getEctdColName("Subject"), 
		timeCol = getEctdColName("Time"), 
		doseCol = getEctdColName("Dose"), 
		replicateCol = getEctdColName("Replicate"), 
		respType = c("Continuous", "Categorical"), 
		catType = c("Proportion", "Count"), 
		fillRespRange = TRUE,
		digits = 3)
{
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# calculateSimTrialMeans.R 10NOV09
	# 
	# Author: Rich
	###############################################################################
	# DESCRIPTION: Create set of simulated trial means
	###############################################################################

	# Check response and categorical types
	respType <- match.arg(respType)
	catType <- match.arg(catType)
	
	# Basic Input Checks
	.checkLogical(diffBase, fillRespRange)											# Check single logicals
	.checkCharacter(respCol, idCol, doseCol, replicateCol, respType, catType)		# Check single characters
	.checkNumeric(digits)															# Check single numerics
	
	# Check inputs & variables
	if (!is.data.frame(data)) ectdStop("Data frame must be provided as the first input")
	neededCols <- c(replicateCol, idCol, doseCol, respCol)
	if (!all(neededCols %in% names(data))) ectdStop("Some variables not found in input data")
	if (!(timeCol %in% names(data))) timeCol <- c()

	# Differences from baseline
	if (diffBase) data <- calculateDiffsFromBase(data, respCol = respCol, idCol = idCol, timeCol = timeCol, replicateCol = replicateCol)
	if (!missing(subset) && length(subset)) try(data <- .applyDataSubset(data, subset))
	
	# Split based on response type
	if (respType == "Continuous") {
		# First pass - include trial, subject, dose, time and by variables
		calcCols <- unique(c(replicateCol, idCol, doseCol, timeCol, bVar))
		calcCols <- calcCols[calcCols %in% names(data)]
		fullMeans <- .dataAggregate(data[respCol], data[calcCols], mean)
		# Second pass - include trial and by variables
		calcCols <- unique(c(replicateCol, bVar))
		calcCols <- calcCols[calcCols %in% names(data)]
		trialMeans <- .dataAggregate(fullMeans[respCol], fullMeans[calcCols], mean)
		trialMeans[[respCol]] <- round(trialMeans[[respCol]], digits)
		return(trialMeans)
	}
	else {
		# Categorical response
		# Partial lengths
		calcCols <- unique(c(replicateCol, bVar))
		calcCols <- calcCols[calcCols %in% names(data)]
		uniResp <- unique(data[[respCol]])
		if (fillRespRange && all(is.integer(uniResp))) uniResp <- min(uniResp):max(uniResp)
		partialN <- .dataAggregate(list("COUNT" = data[[respCol]]), data[c(calcCols, respCol)], function(x) sum(!is.na(x)))

		# Expanded lengths
		uniData <- unique(partialN[calcCols])
		repData <- uniData [ rep(1:nrow(uniData), length(uniResp)), , drop = FALSE]
		mergeData <- cbind(repData, .RESP = rep(uniResp, each = nrow(uniData)))
		names(mergeData) [ names(mergeData) == ".RESP"] <- respCol
		
		fullData <- merge(mergeData, partialN, all = T)
		if (any(naTest <- is.na(fullData$COUNT))) {
			fullData$COUNT <- replace(fullData$COUNT, naTest, 0)
		}

		# Convert if Proportions required
		if (catType == "Proportion") {
			totalData <- .dataAggregate(list(.TOTAL = fullData$COUNT), fullData[calcCols], sum)
			fullData <- merge(fullData, totalData)
			fullData$PROPORTION <- 100 * fullData$COUNT / fullData$.TOTAL
			fullData$PROPORTION <- round(fullData$PROPORTION, digits)
			fullData <- fullData[setdiff(names(fullData), c("COUNT", ".TOTAL"))]
		}

		# Sort the data
		fullData <- fullData [ do.call("order", fullData[c(calcCols, respCol)]), , drop = FALSE ]
		return(fullData)
	}
}
