"calculateObsSummary" <- function(
		data,
		respCol = getEctdColName("Response"),
		bVar = NULL,
		subset = NULL,
		alpha = 95,
		digits = 3,
		diffBase = FALSE, 
		doseCol = getEctdColName("Dose"), 
		timeCol = getEctdColName("Time"), 
		idCol = getEctdColName("Subject"),
		respType = c("Continuous", "Categorical"),
		catType = c("Count", "Proportion"),
		fillRespRange = TRUE
)
{
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# createObsSummary.R 19NOV09
	# 
	# Author: Rich
	###############################################################################
	# DESCRIPTION: Create summary of observed variables from a NONMEM object
	###############################################################################
	# Basic input checks
	respType <- match.arg(respType); catType <- match.arg(catType)
	.checkLogical(diffBase, fillRespRange)							# Check single logicals
	.checkCharacter(respCol, idCol, respType, catType)				# Check single characters
	.checkNumeric(digits)											# Check single numerics
	
	# Parse the "alpha" input
	alpha <- checkSimAlpha(alpha)
	alpha <- qnorm((1 + alpha)/2)
	
	# Get the data
	data <- switch(class(data), 
			"NMRun" = data,   ## TODO: Call correct data method here
			"data.frame" = data,
			ectdStop("Observed data must be either a data frame or NONMEM run object"))
	
	# Check inputs
	if (!(timeCol %in% names(data))) timeCol <- NULL
	if (!(doseCol %in% names(data))) doseCol <- NULL
	if (!length(bVar)) ectdStop("Need at least 1 by variable to calculate summaries")
	needCols <- unique(c(respCol, bVar, idCol))
	checkColNames(names(data), needCols)
	
	# Subset if required
	if (!is.null(subset)) data <- .applyDataSubset(data, subset)
	if (diffBase & length(timeCol)) data <- calculateDiffsFromBase(data, respCol = respCol, idCol = idCol, timeCol = timeCol, replicateCol = NULL)
	
	# Calculate "means" based on response variable type
	if (respType == "Continuous") {
		# Calculate means by subject, time and by variables
		meanData <- .dataAggregate(data[respCol], data[unique(c(idCol, doseCol, timeCol, bVar))], mean, na.rm = TRUE)
		
		#
		# Set up functions to use in the aggregate calls
		#
		loFun <- function(x, alp, dG) round(mean(x, na.rm = T) - (alp * sd(x, na.rm = T))/sqrt(sum(!is.na(x))), dG)
		upFun <- function(x, alp, dG) round(mean(x, na.rm = T) + (alp * sd(x, na.rm = T))/sqrt(sum(!is.na(x))), dG)
		meanFun <- function(x, dG) round(mean(x, na.rm = T), dG)
		medianFun <- function(x, dG) round(median(x, na.rm = T), dG)
		minFun <- function(x, dG) round(min(x, na.rm = T), dG)
		maxFun <- function(x, dG) round(max(x, na.rm = T), dG)
		
		#
		# Create summaries
		#
		myMeds <- .dataAggregate(list(Median = meanData[[respCol]]), meanData[bVar], medianFun, dG = digits)
		myMeans <- .dataAggregate(list(Mean = meanData[[respCol]]), meanData[bVar], meanFun, dG = digits)
		myLower <- .dataAggregate(list(Lower = meanData[[respCol]]), meanData[bVar], loFun, dG = digits, alp = alpha)
		myUpper <- .dataAggregate(list(Upper = meanData[[respCol]]), meanData[bVar], upFun, dG = digits, alp = alpha)
		myN <- .dataAggregate(list(N = meanData[[respCol]]), meanData[bVar], function(x) sum(!is.na(x)))
		myMin <- .dataAggregate(list(Min = meanData[[respCol]]), meanData[bVar], minFun, dG = digits)
		myMax <- .dataAggregate(list(Max = meanData[[respCol]]), meanData[bVar], maxFun, dG = digits)
		
		# Merge summaries
		outData <- merge(myMeds, merge(myMeans, merge(myLower, merge(myUpper, merge(myMin, merge(myMax, myN))))))
	}
	else {
		# Calculate counts by subject, time, by variables AND response
		meanData <- .dataAggregate(list(COUNT = data[[respCol]]), data[unique(c(bVar, respCol))], function(x) sum(!is.na(x)))
		# "Expand" this to a full set of responses
		expData <- unique(data[bVar])
		uniResp <- unique(data[[respCol]])
		if (fillRespRange & all(is.integer(uniResp))) uniResp <- min(uniResp):max(uniResp)
		fullData <- expData[ rep(1:nrow(expData), length(uniResp)), , drop = FALSE]
		fullData[[respCol]] <- rep(uniResp, each = nrow(expData))
		meanData <- merge(meanData, fullData, all = TRUE)
		outData <- meanData [ do.call("order", meanData[unique(c(bVar, respCol))]), , drop = FALSE]
		outData$COUNT <- replace(outData$COUNT, is.na(outData$COUNT), 0)
		if (catType == "Proportion") {
			totalData <- .dataAggregate(list(TOTAL = outData$COUNT), outData[bVar], sum, na.rm = TRUE)
			outData <- merge(outData, totalData)
			outData$PROPORTION <- round(100 * outData$COUNT / outData$TOTAL, digits = digits)
			outData <- outData[setdiff(names(outData), c("COUNT", "TOTAL"))]
		}		
	}
	return(outData)
}
