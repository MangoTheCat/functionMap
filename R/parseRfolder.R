
##' Parse the R scripts in a folder and return the function structure
##'
##' @title Parse the R scripts in a folder and return the function structure
##' @param rpath The path of the R script folders or files.
##' @param rfilepattern An optional regular expression. Only file names which match the regular expression will be parsed.
##' @return A list of the functions structure. 
##' @author Mango Solutions
##' @examples \dontrun{
##' rpath <- system.file("examples", "R", package = "functionMap")
##' parseRfolder(rpath)
##' }
##' 

parseRfolder <- function(rpath, rfilepattern = "\\.[R|r]$", returnfilename = FALSE) {
	rfiles <- character(0)
	isdir <- file.info(rpath)$isdir
	for (i in 1:length(isdir)) {
		if (identical(isdir[i], TRUE)) rfiles <- c(rfiles, list.files(rpath[i], pattern = rfilepattern, full.names = TRUE))
		if (identical(isdir[i], FALSE)) rfiles <- c(rfiles, rpath[i])
	}
	
	if (length(rfiles) == 0) stop("There is no R script to be parsed!")
	
	tmp.func <- list()
	for (i in 1:length(rfiles)) {
		tmp.parse <- try(parseRscript(rfiles[[i]]), silent = TRUE)
		if (!inherits(tmp.parse, "try-error")) tmp.func[[i]] <- tmp.parse
	}
	if (returnfilename) {
		names(tmp.func) <- basename(rfiles)
	} else {
		tmp.func <- rev(do.call("c", tmp.func))
		tmp.func <- rev(tmp.func[unique(names(tmp.func))])
	}
	
	return(tmp.func)
}



