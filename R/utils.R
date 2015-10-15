
.recurseIdx <- function(vidx, netm) {
	tmp <- unique(c(vidx, unique(as.vector(unlist(sapply(vidx, function(x) which(netm[x, ] != 0)))))))
	if (length(vidx) == length(tmp)) {
		returnthis <- tmp
	} else {
		returnthis <- .recurseIdx(tmp, netm)
	}
	return(returnthis)
}

.matchPos.x <- function(x){
	X <- tryCatch({
				as.numeric(x)
			},warning = function(w){
				match.arg(x,c("center", "left", "right"))
			})
	return(X)
}

.matchPos.y <- function(y){
	Y <- tryCatch({
				as.numeric(y)
			},warning = function(w){
				match.arg(y,c("bottom", "center", "top"))
			})
	return(Y)
}

#' @importFrom tools file_ext

.recharts.httpd.handler <- function (path, query, ...) 
{
	path <- gsub("^/custom/recharts/", "", path)
	f <- sprintf("%s%s%s", tempdir(), .Platform$file.sep, path)
	
	ext <- file_ext(path)
	contenttype <- switch(ext,
			"css" = "text/css",
			"gif" = "image/gif",
			"jpg" = "image/jpeg",
			"png" = "image/png",
			"svg" = "image/svg+xml",
			"html" = "text/html",
			"pdf" = "application/pdf",
			"ps" = "application/postscript", # in GLMMGibbs, mclust
			"sgml" = "text/sgml", # in RGtk2
			"xml" = "text/xml",  # in RCurl
			"text/plain")
	
	list(file = f, "content-type" = contenttype, "status code" = 200L)
}



.tempId <- function(){
	id = paste('ID', format(Sys.time(), "%Y%m%d%H%M%S"), proc.time()[3]*100, sep="_")
	return(id)
}


.rechartsOutput <- function(jsonStr, charttype="default", size=c(1024,768)){
	templatedir = getOption("recharts.template.dir")
	chartid <- paste(charttype, basename(tempfile(pattern = "")), sep = "ID")
	
	headerHtml <- readLines(file.path(templatedir, "header.html"))
	footerHtml <- readLines(file.path(templatedir, "footer.html"))
	captionHtml <- readLines(file.path(templatedir, "caption.html"))
	chartHtml <- readLines(file.path(templatedir, "chart.recharts.debug.html"))
    if (isTRUE(getOption('Echart-NODEBUG'))) {
	    chartHtml <- readLines(file.path(templatedir, "chart.recharts.html"))
    }
	
	headerStr <- gsub("HEADER", chartid, headerHtml)
	footerStr <- footerHtml
	captionStr <- gsub("CHARTID", chartid, captionHtml)
	chartStr <- gsub("TEMPID", chartid, chartHtml)
	plotCSS <- sprintf("width:%spx; height:%spx;", size[1],size[2])
	chartStr <- gsub("DIVSIZE", plotCSS, chartStr)
	chartStr <- gsub("<!--JSONHERE-->", jsonStr, chartStr)
	
	headerStr <- paste(headerStr, collapse = "\n")
	footerStr <- paste(footerStr, collapse = "\n")
	captionStr <- paste(captionStr, collapse = "\n")
	chartStr <- paste(chartStr, collapse = "\n")
	
	outList <- list()
	outList$type <- charttype
	outList$chartid <- chartid
	outList$html <- list(header = headerStr, chart = chartStr, caption = captionStr, footer = footerStr)
	
	class(outList) <- c("recharts", charttype, "list")
	return(outList)
	
}

.HTML.escape <- function(x) {
    # only & not used as HTML tag can be replaced
    x <- gsub('&(?![a-z]+;)','&amp;', x , perl=TRUE)
    x <- gsub(' +','&nbsp;',x)
    x <- gsub('"','&quot;',x, fixed=TRUE)
    x <- gsub('<','&lt;',x, fixed=TRUE)
    x <- gsub('>','&gt;',x, fixed=TRUE)
    x
}

.Trim.too.long <- function(x, width=60) {
    # trim too long line
    if(length(ind<-which(nchar(x)>width))){
        x[ind] <- 'Very long expression'#paste(substring(x[ind], 1, width-3), '...')
    }
    x
}

