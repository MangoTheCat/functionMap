
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

plotFunctionMap <- function(networkobj, pdffile, 
		displayisolates = FALSE, displaylabels = TRUE, 
		label.cex = 1, arrowhead.cex = 0.8, edge.lwd = 0.1,
		vertex.col = rep(2, networkobj$gal$n), width=5, height=5) 
{	
	if (!missing(pdffile)) pdf(file = pdffile, width = width, height = height)
	plot(networkobj, displayisolates = displayisolates, displaylabels = displaylabels, 
			boxed.labels = TRUE, arrowhead.cex = arrowhead.cex, edge.lwd = edge.lwd, 
			label.cex = label.cex, vertex.col = vertex.col) 
	if (!missing(pdffile)) dev.off()
}





