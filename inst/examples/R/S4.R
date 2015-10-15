
setClass("Instrument", representation("VIRTUAL", tune = "character")) 		
setClass("Stringed", representation("Instrument"))
setClass("Wind", representation("Instrument"))
setClass("Brass", contains = "Wind") 
setClass("Woodwind", representation(tune = "character"))

setMethod("show", "Instrument", function (object) print(object@tune))

#setGeneric("play1", 
#		function(object, ...) standardGeneric("play"))

#setMethod("play1", "Instrument", function (object) print(paste("Play1:", object@tune)))

play2 <- function(object)
{
	stop(msg = "This method is not implemented for this class!")
}

setGeneric("play2")

play2.Stringed <- function(x) {
	print("I am a Stringed")
}

setMethod("play2", signature(obj = "Stringed"), play2.Stringed)



