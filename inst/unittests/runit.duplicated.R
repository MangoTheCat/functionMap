
test.duplicated <- function() {
	rfolder <- system.file("examples", "R", package = "functionMap")
	lfun1 <- parseRfolder(rfolder)
	#lfun2 <- parseRfolder(rfolder, returnfilename = TRUE)
	
	checkEquals(length(which(names(lfun1) == "fib")), 1)
}
