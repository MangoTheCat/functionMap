
##' Parse the R script and return the function structure
##'
##' @title Parse the R script and return the function structure.
##' @param rfile Path of the R script file.
##' @return A list of all the functions included in this script. Each component contains the names of the functions were called by this function. 
##' @author Mango Solutions
##' @examples \dontrun{
##' rfile <- system.file("examples", "MSToolkit", "R", "utils.R", package = "functionMap")
##' parseRscript(rfile)
##' }
##' 

parseRscript <- function(rfile) {
	
	tmp.env <- new.env()
	source(file = rfile, local = tmp.env, keep.source = TRUE)
	rfile.obj <- ls(tmp.env, all.names = TRUE)
	rfile.fun <- rfile.obj[sapply(rfile.obj, FUN = function(X) class(tmp.env[[X]]) == "function")]
	
	tmp.funcall <- list()
	tmp.dir = tempdir()
	for (i in 1:length(rfile.fun)) {
		tmp.file <- tempfile(tmpdir = tmp.dir)
		tmp.con <- file(tmp.file, open = "w")
		sink(file = tmp.con, type = "output")
		print(body(get(rfile.fun[i], envir = tmp.env)))
		sink(type = "output")
		close(tmp.con)
		tmp.parsedata <- getParseData(parse(tmp.file, keep.source = TRUE))
		tmp.funcall[[i]] <- tmp.parsedata$text[tmp.parsedata$token == "SYMBOL_FUNCTION_CALL"]
		unlink(tmp.file, force = TRUE)
	}
	names(tmp.funcall) <- rfile.fun
	rm(tmp.env)

	return(tmp.funcall)
}



