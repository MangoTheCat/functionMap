"performAnalysis" <- function(
  analysisCode,  							#@ File containing the actual analysis code to run on the data
  doses,         							#@ Doses for which estimates are expected
  data,          							#@ Input dataset
  software = c("R","SAS"),      			#@ Software for analysis: R or SAS
  includeRows = NULL,   					#@ Changes to be made to the data before analysis
  interimCol = getEctdColName("Interim"), 	#@ Interim variable name
  doseCol = getEctdColName("Dose"), 	    #@ Dose variable name
  seed = .deriveFromMasterSeed(), #@ Random number generation seed
  workingPath = getwd(),
  cleanUp = TRUE,
  tempSasDir = tempdir()
)
{
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # performAnalysis.R Wed Jun 27 11:08:20 BST 2007 @464 /Internet Time/
  #
  # Author: Romain    
  ###############################################################################
  # DESCRIPTION: analyze a single replicate of data
  # KEYWORDS: component:analysis
  ###############################################################################

  ## check that the software 
  software <- try( match.arg(software), silent = TRUE )
  if (class(software) == "try-error") ectdStop("The software should be `R` or `SAS`")

  switch( software, 
    "SAS" = {

		# Export the data to a file for SAS to use
		infile <- file.path(tempSasDir, "sasDataInput.csv")
		outfile <- file.path(tempSasDir, "sasDataOutput.csv")
		write.csv(data, file = infile)			# Export data so SAS can read it
		if (!file.exists(infile)) ectdStop("Cannot write replicate to an external CSV file")
				
		# Set up parameters for the SAS call
		sasChanges <- convertSASIncludeRows(includeRows, doseCol = doseCol, interimCol = interimCol) 	# SAS 'keep rows' string
		sasParameters <- paste(infile, outfile, file.path(workingPath, analysisCode), sasChanges, seed, sep="#")
		.log(paste("Calling SAS with execution string \"", sasParameters, "\"", sep=""))

		# Try to call SAS
		trySas <- .ectdSasCall(sasParameters)
		if (class(trySas) == "try-error") ectdStop("Problems occurred when calling SAS in batch mode (see SAS log file for more details)")

		# Has the call been successful?
		if (file.exists(outfile)) {
			sasData <- try(read.csv(outfile))
			if (class(sasData) == "try-error") ectdStop("Could not import SAS analysis output")
			sasData <- checkMicroFormat( sasData, doseCol = doseCol )
         	if (cleanUp) {
				try(file.remove(outfile))
		 		try(file.remove(infile))
			}
		}
		else sasData <- NULL
		
		return(sasData)
    },
    "R" = {
		# Set the seed
		set.seed( seed )

		# Get vector of doses for which statistics are expected
		if (missing(doses)) doses <- unique( data[[doseCol]] )

      	## apply the includeRows change if needed
      	if (!is.null(includeRows)) data <- .keepAnalysisRows(data, includeRows, interimCol, doseCol)

		## run the analysis code
		if (class(analysisCode) == "function") analysisOutput <- try( analysisCode(data)  , silent = TRUE )
		else {
			if (length(analysisCode) == 1 && file.exists(file.path(workingPath, analysisCode))) { # Script
				analysisOutput <- try(source(file.path(workingPath, analysisCode), local = TRUE)$value, silent = TRUE)
				if (class(analysisOutput) == "try-error") ectdStop(paste("Could not perform analysis using script", file.path(workingPath, analysisCode)))			
			}
			else { 	# Character parse
				analysisOutput <- try(eval(parse(analysisCode)), silent = TRUE)
			}
		}
		out <- if( class(analysisOutput) == "try-error" ) {
			ectdWarning("Error when executing analysis code: " %.nt% 
				( analysisOutput %-~% "^[^:]*:") %.nt%    # extract the message from the `try`
				"... creating an empty summary file" )

			NULL
      	}
		else {
        	checkMicroFormat( analysisOutput , doseCol = doseCol ) 
    	}
      
      	return( out )
    })

}

".keepAnalysisRows" <- function(data, includeRows, interimCol, doseCol) {
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	#
	# Author: Rich P
	###############################################################################
	# DESCRIPTION: Subsets data based on 2 column matrix
	###############################################################################
	
	# Check the input structure
	if (!is.matrix(includeRows) || ncol(includeRows) != 2) ectdStop("'includeRows' input should be a matrix with 2 columns")

	# Function for pasting subsets
	"innerPasteSubset" <- function(vec, interimCol, doseCol) paste("(", interimCol, "==", vec[1], "&", doseCol, "==", vec[2], ")")

	# Perform the subset creation
	outSub <- apply(includeRows, 1, innerPasteSubset, interimCol = interimCol, doseCol = doseCol)
	outSub <- paste(outSub, collapse = " | ")
	
	# Try to apply the subset
	myTest <- try(with(data, eval(parse(text = outSub))))
	if (class(myTest) == "try-error" || !is.logical(myTest)) ectdStop("Could not execute subset statement on data")
	if (!any(myTest)) ectdStop("No data to analyze following dose dropping")
	data [ myTest, , drop = FALSE ]
}
