generateData <- function(
		 replicateN ,	                              #@  Number of replicates
		 subjects = NULL,	                          #@ Number of subjects in simulation
		 treatSubj = subjects,	                      #@ Number of subjects to which to allocate treatments, or a vector of allocations
		 treatDoses ,	                              #@ Treatment doses
		 treatSeq ,	                                  #@ Treatment matrix for crossover designs
		 treatType = "Parallel",	                  #@ Treatment type: Parallel or Crossover
		 treatPeriod ,	                              #@ Treatment time points
		 genParNames ,	                              #@ Names of fixed effects to generate
		 genParMean ,	                              #@ Means for generating fixed parameters
		 genParVCov  = 0,	                          #@ Covariance matrix for generating fixed parameters
		 respEqn ,	                                  #@ Formula for creating response
		 respName = getEctdColName("Response"),	      #@ Response variable name
		 treatProp ,                                  #@ Proportions for sampling
		 treatOrder = FALSE,	                      #@ Logical flag: should allocations be assigned in order
		 conCovNames ,	                              #@ Continuous covariate names
		 conCovMean ,	                              #@ Continuous covariate means
		 conCovVCov ,	                              #@ Continuous covariate covariance matrix
		 conCovCrit = NULL,	                          #@ Continuous covariate acceptable range
		 conCovDigits = 3,	                          #@ Continuous covariate rounding digits
		 conCovMaxDraws = 100,	                      #@ Continuous covariate maximum draws
		 disCovNames ,	                              #@ Discrete covariate names
		 disCovVals ,	                              #@ Discrete covariate values
		 disCovProb ,	                              #@ Discrete covariate probabilities
		 disCovProbArray ,	                          #@ Array of probabilities for multivariate sampling
		 extCovNames ,	                              #@ Names for the continuous covariates
		 extCovFile ,	                              #@ File from which to import (including full or relative path)
		 extCovSubset ,	                              #@ Subset to apply to data
		 extCovRefCol ,	                              #@ Reference variable
		 extCovSameRow = TRUE,	                      #@ Logical flag: should covariates sampled be from the same row
		 extCovDataId = idCol,	                      #@ Subject variable name from file
		 timeCovNames ,	                              #@ Time-varying covariate names
		 timeCovMean ,	                              #@ Time-varying covariate means
		 timeCovVCov ,	                              #@ Time-varying covariate covariance
		 timeCovCrit = NULL,	                      #@ Time-varying covariate acceptable range   
		 genParCrit,	                              #@ Range of acceptable values for generated parameters
		 genParBtwNames ,	                          #@ Between subject effects to generate
		 genParBtwMean ,	                          #@ Means for generated between subject effects
		 genParBtwVCov ,	                          #@ Covariance matrix for generated between subject effects
		 genParErrStruc = "None",	                  #@ Function to map generated effects: Additive, Proportional or None
		 genParMaxDraws = 100,	                      #@ Maximum number of iterations to generate valid parameters
		 genParRangeTolerance = .5,					  #@ Proportion of subjects with "in range" parameters that we'd be happy proceeding with
		 extParFile ,	                              #@ File name for external parameter data to import
		 extParNames ,	                              #@ Names of parameters to import from external file
		 extParBtwNames ,	                          #@ Between subject effects variables to import from external file
		 extParBtwNums , 	                          #@ Integer mapping between random and fixed effects in imported parameter data
		 extParSubset = NULL,	                      #@ Subsets to be applied to imported parameter before sampling
		 extParCrit,	                              #@ Range of acceptable values for generated parameters
		 extParErrStruc = "None",	                  #@ Function to map effects from imported parameter data: Additive, Proportional or None
		 extParRefColData ,	                          #@ Reference column in imported parameter data
		 extParRefColName ,	                          #@ Reference column name from imported parameter data
		 extParDataId = idCol,                        #@ Subject variable in external parameter file
		 respInvLink,	                              #@ Inverse link function for the linear predictor
		 respDist = "Normal",	                      #@ Outcome response variable distribution
		 respVCov ,	                                  #@ Residual error (co)variance to apply to generated response
		 respErrStruc = "Additive",	                  #@ Function describing how to apply residual error to the generated response: Additive or Proportional
		 respCrit,	                                  #@ Range of acceptable values for created response
		 respDigits = 3,	                          #@ Number of digits to which to round the created response
		 mcarProp = 0,	                              #@ Proportion of observations to set to missing at random
		 mcarRule,	                                  #@ Rule to specify which observations of the data should be included for MCAR allocation
		 dropFun ,	                                  #@ User defined function to define criteria for subject dropout
		 dropFunExtraArgs = list(),	                  #@ Additional arguments to the dropout function
		 interimSubj ,	                              #@ Proportion of total subjects to be assigned to each interim analysis
		 interimMethod = "Sample",	                  #@ Method for creating interim variable: Sample or Proportion
         seed = .deriveFromMasterSeed(),              #@ random seed
		 idCol = getEctdColName("Subject"),	          #@ Subject variable name
		 doseCol = getEctdColName("Dose"),	          #@ Dose variable name
		 timeCol = getEctdColName("Time"),	          #@ Time variable name
		 trtCol = getEctdColName("Trt"),	          #@ Treatment variable name
		 parOmitFlag = getEctdColName("ParOmit"),	  #@ Parameter omit flag name
		 respOmitFlag = getEctdColName("RespOmit"),   #@ Response omit flag name
		 missingFlag = getEctdColName("Missing"),     #@ Missingness flag name
		 interimCol = getEctdColName("Interim"),      #@ Interim variable name
		 parBtwSuffix = ".Between",                   #@ Suffix for retained between subject effects variables
		 deleteCurrData = TRUE,                       #@ Should existing data be deleted before starting generation phase
		 covDiff = TRUE,                              #@ Should covariates differ between replicates
		 treatDiff = TRUE,                            #@ Should treatment allocation differ between replicates
		 workingPath = getwd()                        #@ Working directory from which to create data
){
 	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# generateData.R Mon Jul 02 15:14:30 BST 2007 @447 /Internet Time/
	#
	# Author: Richard
	###############################################################################
	# DESCRIPTION: High level function to generate simulated trial data
	# KEYWORDS: high, generate
	###############################################################################
	
	# Reset column names
	resetEctdColNames()
	
	# TODO: Better way of extracting default arguments to this function
	defNames <- c("treatSubj", "treatType", "respName", "treatOrder", "conCovCrit", "conCovDigits", "conCovMaxDraws", "extCovSameRow", "extCovDataId", "timeCovCrit", 
		"genParErrStruc", "respDist", "respErrStruc", "respDigits", "genParVCov", "mcarProp", "dropFunExtraArgs", "interimMethod", "seed", "idCol", "doseCol", "timeCol",
    	"trtCol", "parOmitFlag", "respOmitFlag", "missingFlag", "interimCol", "parBtwSuffix", "deleteCurrData", "covDiff", "treatDiff", "extParDataId")
  	callNames <- union(names(match.call())[-1], defNames)

	## Inner Function - Creates call lists from a vector of argument maps
	innerCallList <- function(Vec) {
		Vec <- Vec[Vec %in% callNames]
		if (length(Vec)) lapply(Vec, get, envir=parent.frame()) else list()
	}

	## Look at the values of treatSubj and subjects
	if (missing(subjects) & missing(treatSubj)) ectdStop("One of 'subjects' or 'treatSubj' must be supplied")
	if (missing(subjects)) subjects <- sum(treatSubj)
	if (length(subjects) != 1) ectdStop("The 'subjects' input must contain a single value")
	if (sum(treatSubj) != sum(subjects)) {
		if (sum(treatSubj) < sum(subjects)) {
			if ((sum(subjects) %% sum(treatSubj)) == 0) {
				if (length(treatSubj) == 1) treatSubj <- rep(treatSubj, sum(subjects) %/% sum(treatSubj))
			}
			else ectdStop("Values of 'subjects' and 'treatSubj' are incompatible.  'subjects' is not divisible by sum('treatSubj')")
		}
		else ectdStop("Values of 'subjects' and 'treatSubj' are incompatible. The value of 'subjects' is less than sum('treatSubj')")
	}

  	## Set Argument calling lists: matching of arguments
  	treatList <- innerCallList(c(doses = "treatDoses", times = "treatPeriod", type = "treatType", sequence = "treatSeq", doseCol = "doseCol", timeCol = "timeCol", trtCol = "trtCol"))
  	allocateList <- innerCallList(c(subjects = "treatSubj", prop = "treatProp", ordered = "treatOrder", idCol = "idCol", trtCol = "trtCol"))
  	covList <- innerCallList(c(subjects = "subjects", conNames = "conCovNames", conMean = "conCovMean", conCov = "conCovVCov", conRange = "conCovCrit", conDigits = "conCovDigits", 
    	conMaxDraws = "conCovMaxDraws", disNames = "disCovNames", disValues = "disCovVals", disProbs = "disCovProb", disProbArray = "disCovProbArray", extNames = "extCovNames", 
    	extFile = "extCovFile", extSubset = "extCovSubset", extRefCol = "extCovRefCol", extSameRow = "extCovSameRow", extDataId = "extCovDataId", idCol = "idCol", workingPath = "workingPath",
		timeNames = "timeCovNames", timeMean = "timeCovMean", timeCov = "timeCovVCov", timeRange = "timeCovCrit", timeCol = "timeCol", timePeriod = "treatPeriod"))
	parList <- innerCallList(c(subjects = "subjects", genNames = "genParNames", genFixedMean = "genParMean", 
		genFixedCov = "genParVCov", genRange = "genParCrit", genBetweenNames = "genParBtwNames", genBetweenMean = "genParBtwMean", genBetweenCov = "genParBtwVCov", 
		genErrStruc = "genParErrStruc", genMaxDraws = "genParMaxDraws", genParRangeTolerance = "genParRangeTolerance", extFile = "extParFile", extNames = "extParNames", extBetween = "extParBtwNames", extBetweenNums = "extParBtwNums", extSubset = "extParSubset", 
    	extRange = "extParCrit", extErrStruc = "extParErrStruc", extRefCol = "extParRefColData", extRefColName = "extParRefColName", extDataId = "extParDataId", suffix = "parBtwSuffix", idCol = "idCol", 
    	flagName = "parOmitFlag", workingPath = "workingPath"))
	respList <- innerCallList(c(equation = "respEqn", name = "respName", invLink = "respInvLink", distribution = "respDist", covariance = "respVCov", errStruc = "respErrStruc", 
		range = "respCrit", digits = "respDigits", flagName = "respOmitFlag"))
	mcarList <- innerCallList(c(prop = "mcarProp", rule = "mcarRule", flagName = "missingFlag" ))

	dropList <- innerCallList(c(dropFunc = "dropFun", idCol = "idCol", timeCol = "timeCol", flagName = "missingFlag"))
	interimList <- innerCallList(c(subjects = "subjects", proportion = "interimSubj", idCol = "idCol", interimCol = "interimCol", method = "interimMethod"))

	## Set directory structures
	if (deleteCurrData) removeDirectories("ReplicateData", workingPath = workingPath)
	createDirectories("ReplicateData", workingPath = workingPath)
  
	## Derive Treatment Data
	treatData <- do.call(createTreatments, treatList)
	allocateList$trts <- max(treatData[[trtCol]])                          

	## Allocate treatments if required
	if (!treatDiff) {
		allocData <- do.call(allocateTreatments, allocateList)
		allocData <- allocData [ rep(1:nrow(allocData), length = subjects), , drop = FALSE]
		allocData[[idCol]] <- 1:subjects
	}
	if (!covDiff) covData <- do.call(createCovariates, covList)
             
  	# Add buffer if we are adding to existing replicates in the directory
	buffer <- if (deleteCurrData) 0 
	else {
		allReps <- try(getReplicates(workingPath = workingPath), silent = TRUE)
		if (class(allReps) == "try-error") buffer <- 0
		else buffer <- max(allReps)
	}
  
	## Loop around replicates
	if (length(replicateN) == 1) replicateN <- 1:replicateN
	for (i in replicateN) {

		## Set low level component seeds
	    allocateList$seed <- seed + 1 * i
	    covList$seed      <- seed + 2 * i
	    parList$seed      <- seed + 3 * i
	    respList$seed     <- seed + 4 * i
	    mcarList$seed     <- seed + 5 * i
	    dropList$seed     <- seed + 6 * i
	    interimList$seed  <- seed + 7 * i	

	    ## Replicate Looping: Core Data Structure
   	 if (covDiff) covData <- do.call(createCovariates, covList)
   	 if (treatDiff) {
		 allocData <- do.call(allocateTreatments, allocateList)
		 allocData <- allocData [ rep(1:nrow(allocData), length = subjects), , drop = FALSE]
		 allocData[[idCol]] <- 1:subjects
	 }
	 
	 if (timeCol %in% names(covData)) bycov <- c(idCol, timeCol) else bycov <- idCol
	 coreData <- merge(merge(treatData, allocData, by=trtCol), covData, by=bycov)
	 sortBy <- c(idCol, trtCol, timeCol, doseCol)
	 sortBy <- sortBy [ sortBy %in% names(coreData) ]
	 if (length(sortBy)) coreData <- coreData [ do.call("order", coreData[sortBy]),,drop=FALSE]
	
   	 ## Replicate Looping: Parameters and Reponse
   	 if (!missing(extParRefColData) && length(extParRefColData) == 1 && is.character(extParRefColData) && !length(grep(",", extParRefColData))) {
   	   if (!length(grep(".refCol", extParRefColData))) extParRefColData <- paste(extParRefColData, ".refCol", sep="")
   	   if (extParRefColData %in% names(coreData)) parList$extRefCol <- coreData[[extParRefColData]]
   	   else parList <- parList[names(parList) != "extRefCol"]      
   	 }
    coreData <- merge(coreData, do.call(createParameters, parList), by=idCol)
    respList$data <- coreData
    respData <- do.call(createResponse, respList)
    if (is.data.frame(respData) && nrow(respData) == nrow(coreData)) coreData <- cbind(coreData, respData) else ectdStop("Cound not create response variable")

    ## Replicate Looping: Flags and Interims
    if (mcarProp > 0) {
      mcarList$data <- coreData
      coreData <- do.call(createMCAR, mcarList)
    }
    if (!missing(dropFun)) {
      dropList$data <- coreData
      coreData <- do.call(createDropout, c(dropList, dropFunExtraArgs))
    }

    if (!missing(interimSubj)) coreData <- merge(coreData, do.call(createInterims, interimList), by=idCol)

    ## Replicate Looping: Exporting Data
    writeData(coreData, i + buffer, dataType = "Replicate", workingPath = workingPath)

    .log( sprintf("gendata replicate %5d / %5d", i, length(replicateN)) )
  }
 
  # Set new default column names
  setEctdColName("Subject", idCol)
  setEctdColName("Dose", doseCol)
  setEctdColName("Time", timeCol)
  setEctdColName("Response", respName)
  setEctdColName("Trt", trtCol)
  setEctdColName("Interim", interimCol)
  setEctdColName("Missing", missingFlag)
  setEctdColName("ParOmit", parOmitFlag)
  setEctdColName("RespOmit", respOmitFlag)
  
  invisible()  
}



