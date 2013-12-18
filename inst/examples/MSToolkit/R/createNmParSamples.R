"createNmParSamples" <- function(
		N, 													#@ Number of samples to create
		run,												#@ NONMEM run
		seed = .deriveFromMasterSeed( ),					#@ Random seed
		method = c("Covariance", "Final", "Initial")		#@ Method for creating samples
) 
{
	# Stop if library not there
	if (!require(RNMImport)) ectdStop("RNMImport library not found")
	
	# Set the seed	
	set.seed(seed)

	## Check inputs
	if (!is.numeric(N) || length(N) != 1 || N < 1) ectdStop("Number of samples must be a single positive integer")
	method <- match.arg(method)

	## Get parameter estimates
	switch(class(run),
		"NMRun" = {
			thVals <- switch(method, "Initial" = RNMImport:::getThetas(run, "initial")[2,], RNMImport:::getThetas(run))
			omVals <- switch(method, "Initial" =  RNMImport:::getOmegas(run, "initial"),  RNMImport:::getOmegas(run))
			sgVals <- switch(method, "Initial" = RNMImport:::getSigmas(run, "initial"), RNMImport:::getSigmas(run))
			theCov <- RNMImport:::getEstimateCov(run)
		},
		"nmRunReport" = {
			if (method == "Initial") ectdStop("For 'initial' method, a control file object or full run must be provided")
			thisProb <- run$problemResults[[1]]
			thVals <- thisProb$FinalEstimates$THETA
			omVals <- thisProb$FinalEstimates$OMEGA
			sgVals <- thisProb$FinalEstimates$SIGMA
			theCov <- thisProb$CovarianceMatrix
		},
		"nmModel" = {
			if (method %in% c("Final", "Covariance")) ectdStop(paste("For '", method, "' method, a report file object or full run must be provided", sep=""))
			thisProb <- run$problem[[1]]
			thVals <- thisProb$Theta[,2]
			omVals <- thisProb$Omega
			sgVals <- thisProb$Sigma
			theCov <- NULL
		}, 
		ectdStop("'run' input must be a NONMEM run object, a NONMEM contol file, or a NONMEM output file, as created by the RNMImport library")
	)

	## Check matrix inputs are symmetricPD
	if (length(omVals)) checkSymmetricPDMatrix(omVals)
	if (length(sgVals)) checkSymmetricPDMatrix(sgVals)

	## Return "Initial" or "Final" samples
	if (method %in% c("Initial", "Final")) {
		basePars <- list(THETA = thVals, OMEGA = omVals, SIGMA = sgVals)
		if (!length(unlist(basePars))) ectdStop(paste("Could not extract", method, "parameters"))
		return(lapply(1:N, function(i, lst) lst, lst = basePars))   # TODO: Better way of doing this?
	}

	## Get lower triangles of OMEGA and SIGMA
	if (length(omVals)) {
		omPos <- lower.tri(omVals, diag = TRUE)
		omVals <- omVals[ omPos ]
	} else omVals <- c()
	if (length(sgVals)) {
		sgPos <- lower.tri(sgVals, diag = TRUE)
		sgVals <- sgVals[ sgPos ]
	} else sgVals <- c()

	## Combine parameters into a single vector
	allPars <- c(thVals, omVals, sgVals)
	if (length(allPars) != nrow(theCov)) ectdStop(paste("Number of parameters (", length(allPars), ") does not match dimensions of covariance matrix (", nrow(theCov), ")", sep=""))

	# Get the elements from the covariance matrix
	isZero <- apply(theCov == 0, 2, all)
	subPars <- allPars[!isZero]
	subCov <- theCov[!isZero,!isZero]
	checkSymmetricPDMatrix(theCov)

	# Simulate Values
	newPars <- matrix(allPars, nrow = N, ncol = length(allPars), byrow = TRUE) 		# Build initial matrix structure
	newPars[,!isZero] <- mvrnorm(N, subPars, subCov)								# Use "mvrnorm" to create samples
	appFun <- function(vec, nTh, thN, nOm, omN, nSg, sgN) {
		newTh <- vec[1:nTh]; names(newTh) <- thN
		newOm <- createNmMatrix(vec[nTh + (1:nOm)], dimnames = omN, byrow = FALSE)
		newSg <- createNmMatrix(vec[nTh + nOm + (1:nSg)], dimnames = sgN, byrow = FALSE)
		list(THETA = newTh, OMEGA = newOm, SIGMA = newSg)
	}
	apply(newPars, 1, appFun, nTh = length(thVals), thN = names(thVals), nOm = length(omVals), omN = dimnames(omVals), nSg = length(sgVals), sgN = dimnames(sgVals))
}
