"getReplicates" <- function(
		path = "ReplicateData", 			#@ Path within which replicates are stored
		prefix = "replicate", 				#@ Prefix for files in the directory
		method = getEctdDataMethod(),		#@ Data storage method
		workingPath = getwd()				#@ Working directory
) {
	.checkCharacter(path, prefix, method, workingPath)		# Check (character) inputs
	switch(method, 
			"CSV" = {
				path <- file.path(workingPath, path)
				if (!file.exists(path)) ectdStop(paste("Could not find path \"", path, "\"", sep=""))
				searchPattern <- paste("^", prefix, ".+[0-9]+\\.[cC][sS][vV]$", sep="")
				fileNames <- casefold(list.files(path, pattern=searchPattern))
				fileNames <- gsub("\\.csv", "", gsub(casefold(prefix), "", fileNames))
				if (!length(fileNames)) ectdStop(paste("No files to read from directory", path))
				replicates <- try(as.numeric(fileNames))
				if (class(replicates) == "try-error") ectdStop(paste("File names in directory", path, "of wrong format"))
			},
			"RData" = {
				path <- file.path(workingPath, path)
				if (!file.exists(path)) ectdStop(paste("Could not find path \"", path, "\"", sep=""))
				searchPattern <- paste("^", prefix, ".+[0-9]+\\.[rR][dD][aA][tT][aA]$", sep="")
				fileNames <- casefold(list.files(path, pattern=searchPattern))
				fileNames <- gsub("\\.rdata", "", gsub(casefold(prefix), "", fileNames))
				if (!length(fileNames)) ectdStop(paste("No files to read from directory", path))
				replicates <- try(as.numeric(fileNames))
				if (class(replicates) == "try-error") ectdStop(paste("File names in directory", path, "of wrong format"))
			},
			"Internal" = {
				if (!length(.ectdEnv$DataStore)) ectdStop("No data to read from internal data store")
				replicates <- 1:length(.ectdEnv$DataStore)
			},
			ectdStop(paste("Data storage method \"", method, "\" not recognised", sep=""))
	)
	replicates
}
