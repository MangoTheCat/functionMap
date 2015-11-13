
test.duplicated <- function() {
	rfolder <- system.file("examples", "R", package = "functionMap")
	lfun1 <- parse_r_folder(rfolder)
	#lfun2 <- parse_r_folder(rfolder, returnfilename = TRUE)
	
	checkEquals(length(which(names(lfun1) == "fib")), 1)
}
