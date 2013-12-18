"analyzeData" <- function(
  replicates = "*",                  #@ Replicates to perform analysis on 
  analysisCode,                      #@ Function taking a data
  macroCode,                         #@ Macro evaluation code
  interimCode = NULL,                #@ Interim analysis code
  software = "R",                    #@ Software for analysis: R or SAS
  grid = FALSE,                       #@ Split analysis across the grid?
  waitAndCombine = TRUE,             #@ Wait for all analyses to finish, then combine into single file?
  cleanUp = FALSE,                   #@ Delete micro/macro directories on completion?
  removeMissing = TRUE,              #@ Remove Missing rows?
  removeParOmit = TRUE,              #@ Remove Parameter Omit rows?
  removeRespOmit = TRUE,             #@ Remove Response Omit rows?
  seed = .deriveFromMasterSeed(),    #@ Random number seed
  parOmitFlag = getEctdColName("ParOmit"),           #@ Parameter omit flag name
  respOmitFlag = getEctdColName("RespOmit"),         #@ Response omit flag name
  missingFlag = getEctdColName("Missing"),           #@ Missing flag name
  interimCol = getEctdColName("Interim"),            #@ Interim variable name
  doseCol = getEctdColName("Dose"),                  #@ Dose variable name
  sleepTime = 15,                    #@ Number of seconds to sleep between checking for grid jobs
  deleteCurrData = TRUE,             #@ Delete current analysis results before executing
  initialDoses = NULL,					#@ Initial doses to use for "Interim 1"
  stayDropped = TRUE,				#@ Dose dropping flag: if a dose is dropped, should it stay dropped?
  fullAnalysis = TRUE,			#@ Perform a full analysis
  workingPath = getwd(),              #@ Working path containing data
  method = getEctdDataMethod()
)
{
	###############################################################################
	# � Mango Solutions, Chippenham SN14 0SQ 2006
	# analyzeData.R Tue Jul 03 16:24:00 BST 2007 @447 /Internet Time/
	#
	# Author: Richard, Romain
	###############################################################################
	# DESCRIPTION: High level function to analyze simulated trial datasets
	# KEYWORDS: high, analyze                                                
	###############################################################################
	# TESTME
	funCall <- match.call()

	## Check network connectivity  
	macroCode <- .checkFun(macroCode, "data")
	replicates <- .checkReplicates( replicates, workingPath = workingPath, method = method)
	
	if (grid && !.checkGridAvailable()) grid <- FALSE
	if (length(replicates) == 1) grid <- waitAndCombine <- FALSE

	## Check directories
	if (deleteCurrData) removeDirectories(c("Micro", "Macro"), workingPath = workingPath)
	createDirectories(c("MicroEvaluation", "MacroEvaluation"), workingPath = workingPath)
  
	## Split jobs and call grid
	if (grid) {
			
		nclusters <- parallel:::detectCores() - 1
		if (is.numeric(getOption("max.clusters"))) nclusters <- min(nclusters, getOption("max.clusters"))
		cl <- parallel:::makeCluster(nclusters)
		stopCluster <- parallel:::stopCluster
		
		repSplit <- .splitGridVector(replicates, ceiling(length(replicates) / nclusters ))

		arguments=list( analysisCode = analysisCode, 
						interimCode = interimCode, 
                        software = software, 
                        removeMissing = removeMissing, 
						removeParOmit = removeParOmit, 
                        removeRespOmit = removeRespOmit, 
						seed = seed,  
                        parOmitFlag = parOmitFlag, 
                        respOmitFlag = respOmitFlag, 
						missingFlag = missingFlag, 
                        interimCol = interimCol, 
                        doseCol = doseCol, 
						initialDoses = initialDoses, 
                        stayDropped = stayDropped, 
                        fullAnalysis = fullAnalysis,
						workingPath = workingPath, 
                        macroCode = macroCode,
                        method = method)
        clusterApply(cl=cl, repSplit, function(l,a){
			for (i in l) {
				microData <- analyzeRep(replicate = i, analysisCode = a$analysisCode, 
						interimCode = a$interimCode, software = a$software, removeMissing = a$removeMissing, 
						removeParOmit = a$removeParOmit, removeRespOmit = a$removeRespOmit, 
						seed = a$seed + i, parOmitFlag = a$parOmitFlag, respOmitFlag = a$respOmitFlag, 
						missingFlag = a$missingFlag, interimCol = a$interimCol, doseCol = a$doseCol, 
						initialDoses = a$initialDoses, stayDropped = a$stayDropped, fullAnalysis = a$fullAnalysis,
						workingPath = a$workingPath, method = a$method)
				
				# Write out data
				if (is.data.frame(microData) && nrow(microData)) {
					
					writeData(microData, i, "Micro", workingPath = a$workingPath)
					
					macroData <- macroEvaluation(microData, macroCode = a$macroCode, 
							interimCol = a$interimCol, doseCol = a$doseCol)
					
					writeData(macroData, i, "Macro", workingPath = a$workingPath)
				}
				else ectdWarning(paste("No return output from replicate", i))
			}
		}, arguments)

		stopCluster(cl)
		evalTime <- Sys.time()                              # Store time at grid evaluation
	} else {
		
		# Loop through and analyze replicates
		for (i in replicates) {

			## TODO: Update analyzeRep and performAnalysis with data storage method ..
			microData <- analyzeRep(replicate = i, analysisCode = analysisCode, 
				interimCode = interimCode, software = software, removeMissing = removeMissing, 
				removeParOmit = removeParOmit, removeRespOmit = removeRespOmit, 
				seed = seed + i, parOmitFlag = parOmitFlag, respOmitFlag = respOmitFlag, 
	        	missingFlag = missingFlag, interimCol = interimCol, doseCol = doseCol, 
				initialDoses = initialDoses, stayDropped = stayDropped, fullAnalysis = fullAnalysis,
				workingPath = workingPath, method = method)
	
			# Write out data
			if (is.data.frame(microData) && nrow(microData)) {
	
				writeData(microData, i, "Micro", workingPath = workingPath)
	      
				macroData <- macroEvaluation(microData, macroCode = macroCode, 
					interimCol = interimCol, doseCol = doseCol)
	      
				writeData(macroData, i, "Macro", workingPath = workingPath)
			}
			else ectdWarning(paste("No return output from replicate", i))
		}
	}
	
	if (waitAndCombine) {   

    	compileSummary("Micro", workingPath = workingPath)
    	compileSummary("Macro", workingPath = workingPath)      
    
	}
	.cleanup( cleanUp = cleanUp, grid = grid, workingPath = workingPath )
  
	invisible()
}

