
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
##' lfun <- parseRfolder(system.file("examples", "R", package = "functionMap"))
##' n1 <- createNetwork(lfun)
##' plotFunctionMap(n1)
##' }
##' @export

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


##' Parse Rd docs get function export information
##' @param packagedir path to root of package
##' @return a data.frame summary for exported (as you documented in Rd by export)
##' @export
parse_roxygen_export = function(packagedir) {
    require(roxygen2)
    rc = namespace_roclet()
    re = roxygen2:::parse_package(normalizePath(packagedir), roxygen2:::source_package)
    ns = roxygen2:::roc_process.namespace(rc, re)
    n1 = sapply(ns, function(x) parse(text=x)[[1]])
    d  = data.frame(name=character(), type=character(), S3.type=character(), stringsAsFactors=FALSE)
    for(e in n1){
        if (e[[1]]=='export') {
            for(j in 2:(length(e))){
                d[NROW(d)+1,]=c(as.character(e[[j]]), 'function', NA)
            }
        } else if (e[[1]]=='S3method') {
                d[NROW(d)+1,]=c(as.character(e[[2]]), 'S3method', as.character(e[[3]]))
        }
    }
    attr(d,'original.information')=n1
    d
}

##' wrapper for plotFunctionMap
##'
##' If the packagedir has Rd docs, it will create colorful plot for vertices
##' @param packagedir path to root of package, also should be the name of package
##' @param relabel re-label to numbers, and add a legend
##' @param do.plot only run this function as a wrapper to create network or actual draw the function map
##' @examples \dontrun{
##'   plotFunctionMap.package('/src/ggplot2', width=10, height=10, displayisolates=TRUE, displaylabels=FALSE)
##'   plotFunctionMap.package('/src/plyr', width=10, height=10, displayisolates=TRUE, label.cex=0.2)
##'   plotFunctionMap.package('/src/foreach', width=10, height=10, displayisolates=TRUE, label.cex=0.2)
##'   # simply use this function to create network only
##'   net. = plotFunctionMap.package('/local/maturity_temp_dir/src/Matrix', do.plot=FALSE)
##' } 
##' @export
plotFunctionMap.package = function(packagedir,relabel=TRUE,do.plot=TRUE,...) {
    pdir = normalizePath(packagedir)
    if (do.plot) {
        rd = try(parse_roxygen_export(pdir), silent=TRUE)
        if (is(rd,'try-error')) {
            rd  = data.frame(name=character(), type=character(), S3.type=character(), stringsAsFactors=FALSE)
        }
        if (NROW(rd)==0) {
            warning('no @export in found in Rd, or no Rd documents, hence no meaningful color in the plot')
        }
    } else {
        rd  = data.frame(name=character(), type=character(), S3.type=character(), stringsAsFactors=FALSE)
    }
    nt = parseRfolder(file.path(packagedir, 'R'))
    # assuming this package is installed
    package.name = read.dcf(file.path(packagedir, 'DESCRIPTION'))[,'Package']
    s4 = try(parseS4fromNs( package.name ), silent=TRUE)
    if (is(s4,'try-error')){
        txts = extract.S4.defn(path=package)
        try(eval(parse(text=txts)), silent=TRUE)
        s4 = try(parseS4fromNs(), silent=TRUE)
    }
    if (is(s4,'try-error')) {
        n1 = createNetwork(nt)
    } else {
        n1 = createDirectedNetwork(nt, s4)
    }
    if (do.plot) {
        v = colnames(n1[,])
        col = rep(2, length(v)) + ( v %in% rd$name[ rd$type=='function' ])
        if (relabel) {
            m = n1[,]
            colnames(m)=NULL
            rownames(m)=NULL
            plot(network(m), vertex.col=col, displaylabels=TRUE, ...)
            legend('topleft', legend=sprintf('%3s %s',1:length(v),v), cex=0.5)
        } else {
            plotFunctionMap(n1, vertex.col=col, ...)
        }
        if (NROW(rd)>0) legend('topright', legend=c('Internal','External'),col=c(2,3),pch=20)
    }
    n1
}

