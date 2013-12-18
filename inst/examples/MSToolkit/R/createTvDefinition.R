# Create the tvDefinition class
setClass("tvDefinition", 
	representation(Name = "character", Value = "numeric", trtCall = "list"),
	validity = function(object) {
		test1 <- length(object@Name) == 1
		test2 <- length(object@Value) == 1 && object@Value %in% 0:999
		test3 <- all(names(object@trtCall) %in% c("doses", "times", "type", "sequence"))
		test4 <- test3 && object@trtCall$type %in% c("Parallel", "Crossover")
		if (!test1) return("Drug name must be a character string of length 1")
		if (!test2) return("Drug value must be a single positive integer")
		if (!test3) return("Treatment call is of the wrong format")
		if (!test4) return("Dosing method should be either 'Parallel' or 'Crossover'")
		TRUE
	}
)

"createTvDefinition" <- function(
	drugName,								#@ Name of drug  
	drugValue,								#@ Value to be associated with this drug  
	doses,									#@ Vector of dose values  
	times = NULL,							#@ Optional vector of times  
	type = c("Parallel", "Crossover"),  	#@ Dosing Method: "Parallel" (default) or "Crossover"
	sequence,								#@ Optional sequence matrix for crossover trials
	testCall = TRUE
){
	if (missing(drugName)) ectdStop("Must provide a drug name")
	if (missing(drugValue)) ectdStop("Must provide a drug value")
	if (missing(doses)) ectdStop("Must provide a vector of doses")
	type <- match.arg(type)
	if (type == "Crossover" & missing(sequence)) ectdStop("Sequence matrix must be provided for crossover design")
	tcList <- list(doses = doses, times = times, type = type)
	if (!missing(sequence)) tcList$sequence <- sequence
	tvObject <- new("tvDefinition", Name = drugName, Value = drugValue, trtCall = tcList)
	if (testCall) {
		tryCall <- try(do.call("createTreatments", tvObject@trtCall), silent = TRUE)
		if (class(tryCall) == "try-error") ectdStop("Generated definition fails on test call to 'createTreatments'")
	}
	tvObject
}
