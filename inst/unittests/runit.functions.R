
test.functions <- function() {
	rfile <- system.file("examples", "R", "func.R", package = "functionMap")
	lfun <- parseRscript(rfile)
	checkTrue(is.list(lfun))
}
