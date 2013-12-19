
##' Plot the network of functions
##'
##' @title Plot the network of functions
##' @param networkobj The network object which can be produced by \code{\link{createNetwork}}.
##' @param displayisolates Should isolates be displayed?
##' @param displaylabels Should vertex labels be displayed?
##' @param label.cex Character expansion factor for label text.
##' @param arrowhead.cex Expansion factor for edge arrowheads.
##' @param edge.lwd line width scale for edges.
##' @param vertex.col color for vertices; may be given as a vector or a vertex attribute name, if vertices are to be of different colors.
##' @param pdffile The file name if output PDF file.
##' @param width Width of PDF.
##' @param height Height of PDF.
##' @return NULL. 
##' @author Mango Solutions
##' @examples \dontrun{
##' lfun <- parseRfolder(system.file("examples", "MSToolkit", "R", package = "functionMap"))
##' n1 <- createNetwork(lfun)
##' plotFunctionMap(n1)
##' }
##' 

plotFunctionMap <- function(networkobj, displayisolates = FALSE, displaylabels = TRUE, 
		label.cex = 1, arrowhead.cex = 0.8, edge.lwd = 0.1,vertex.col = rep(2, networkobj$gal$n),
		pdffile,  width = 5, height = 5) 
{	
	if (!missing(pdffile)) pdf(file = pdffile, width = width, height = height)
	plot(networkobj, displayisolates = displayisolates, displaylabels = displaylabels, 
			boxed.labels = TRUE, arrowhead.cex = arrowhead.cex, edge.lwd = edge.lwd, 
			label.cex = label.cex, vertex.col = vertex.col) 
	if (!missing(pdffile)) dev.off()
}





