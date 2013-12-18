"summarizeTrialMeans" <- function(
		data, 
		respCol = getEctdColName("Response"), 
		bVar = getEctdColName("Dose"),
		alpha = 95, 
		digits = 3, 
		method = "Quantile"
)
{
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# summarizeTrialMeans.R 10NOV09
	#
	# Author: Rich
	###############################################################################
	# DESCRIPTION: Summarize a set of trial mean responses
	###############################################################################

	# Check inputs
	if (!is.data.frame(data)) ectdStop("First input must be a data frame")
	if (length(respCol) != 1) ectdStop("Function only valid for a single simulated response")
	if (!(respCol %in% names(data))) ectdStop("Could not find response variables in input dataset")
	if (!length(bVar)) ectdStop("Must provide at least 1 by variable")
	if (!all(bVar %in% names(data))) ectdStop("Could not find all 'by' variables in input dataset")
	method <- match.arg(method, c("Quantile", "Gaussian"))
	alpha <- checkSimAlpha(alpha)	# Parse alpha
	
	# Build basic summary functions to use
	meanFun <- function(x, digits) round(mean(x, na.rm = T), digits)			# Mean function
	medianFun <- function(x, digits) round(median(x, na.rm = T), digits)		# Median function
	minFun <- function(x, digits) round(min(x, na.rm = T), digits)				# Minimum function
	maxFun <- function(x, digits) round(max(x, na.rm = T), digits)				# Maximum function
	nFun <- function(x) sum(!is.na(x))								# Number of non-missings
	
	# Stop if unrecognised summary method
	if(method == "Quantile") {			# Quantile method
		loFun <- function(x, alp, digits) round(quantile(x, prob = (1 - alp)/2, na.rm = T), digits) # "Lower" function
		upFun <- function(x, alp, digits) round(quantile(x, prob = (1 + alp)/2, na.rm = T), digits) # "Upper" function
	}
	else {								# Gaussian method
		loFun <- function(x, alp, digits) {
			naTest <- !is.na(x)
			if(sum(naTest) == 1) round(x[naTest], digits)
			else round(mean(x, na.rm = T) + qnorm((1 - alp)/2) * sd(x, na.rm = T), digits)
		}
		upFun <- function(x, alp, digits) {
			naTest <- !is.na(x)
			if(sum(naTest) == 1) round(x[naTest], digits)
			else round(mean(x, na.rm = T) + qnorm((1 + alp)/2) * sd(x, na.rm = T), digits)
		}
	}

	# Calculate summaries - basic fashion to aid readibiliy
	sumMeans <- .dataAggregate(list(Mean = data[[respCol]]), data[bVar], meanFun, digits = digits )
	sumMedians <- .dataAggregate(list(Median = data[[respCol]]), data[bVar], medianFun, digits = digits )
	sumMin <- .dataAggregate(list(Min = data[[respCol]]), data[bVar], minFun, digits = digits )
	sumMax <- .dataAggregate(list(Max = data[[respCol]]), data[bVar], maxFun, digits = digits )
	sumN <- .dataAggregate(list(N = data[[respCol]]), data[bVar], nFun)
	sumLower <- .dataAggregate(list(Lower = data[[respCol]]), data[bVar], loFun, alp = alpha, digits = digits)
	sumUpper <- .dataAggregate(list(Upper = data[[respCol]]), data[bVar], upFun, alp = alpha, digits = digits)
	simData <- merge(sumMedians, merge(sumMeans, merge(sumLower, merge(sumUpper, merge(sumMin, merge(sumMax, sumN))))))

	# Return (sorted) summary data
	simData[do.call("order", simData[bVar]),  , drop = FALSE]
}

