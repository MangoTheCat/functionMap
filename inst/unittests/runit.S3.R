
test.S3 <- function() {
	rfile <- system.file("examples", "R", "S3.R", package = "functionMap")
	lfun <- parseRscript(rfile)
	checkTrue(is.list(lfun))
}