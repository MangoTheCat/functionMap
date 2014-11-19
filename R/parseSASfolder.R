#' parseSASfolder
#'
#' similar to parseRfolder, but deal with SAS code
#'
#' @param sas.path the toplevel path containing SAS scripts
#' @param sas.pattern the extension pattern for SAS scripts
#' @return list of contained proc and macros
#' @export
#' @examples \dontrun{
#'    nn <- createNetwork(parseSASfolder('.'))
#'    plotFunctionMap(nn)
#'    plot(eForce(nn))
#' }
parseSASfolder <- function(sas.path, sas.pattern='\\.[Ss][Aa][Ss]$') {
    sas.files <- list.files(sas.path, pattern=sas.pattern, rec=TRUE, full.name=TRUE)
    user.macro.list <- sub(sas.pattern,'',casefold(basename(sas.files)))
    L <- list()
    for(fn in sas.files) {
        L[[ casefold(sub(sas.pattern,'',basename(fn))) ]] <- parseSASscript(fn, user.macro.list)
    }
    L
}

#' network.from.sascode
#'
#' A Wrapper for the \code{\link{parseSASfolder}} and \code{\link{createNetwork}}
#' Also set vertex attribute for the toplevel scripts.
#'
#' @param sas.path basedir to analysis
#' @param sas.pattern the extention file name pattern
#' @return network object
#' @export
#' @examples \dontrun{
#'   net <- network.from.sascode('.')
#'   # it will have the atrribute of being a toplevel script or not
#'   net %v% 'toplevel'
#' }
network.from.sascode <- function(sas.path, sas.pattern='\\.[Ss][Aa][Ss]$') {
    l <- parseSASfolder(sas.path, sas.pattern)
    net <- createNetwork(l)
    toplevel.scripts <- 
        casefold(sub(sas.pattern,'', basename(list.files(sas.path, pattern=sas.pattern))))
    # by default set.vertex.attribute is igraph::set.vertex.attribute, which is not what we want
    # here the function need to be fully qualified
    network::set.vertex.attribute(net, 'toplevel', network.vertex.names(net) %in% toplevel.scripts)
    net
}

#' show.connected.components
#'
#' display connected components with more than one node from a graph
#' @param nn network object
#' @param make.plot make the plot for components
#' @return the list components network
#' @export
#' @examples \dontrun{
#'   foo <- parseSASfolder('.')
#'   nn  <- createNetwork(foo)
#'   show.connected.components(nn)
#' }
show.connected.components <- function(nn, make.plot=TRUE) {
    cps <- dfs.matrix.travel(nn[,])
    # components with 
    ind <- which(sapply(cps, length)>1)
    L <- list() 
    for(i in ind) {
        L[[ length(L) + 1]] <- as.network(nn[cps[[i]], cps[[i]]])
        if (make.plot) {
            dev.new()
            plotFunctionMap(L[[ length(L) ]])
        }
    }
    invisible(L)
}

#' toplevel.sas.structure
#'
#' Analysis and visualize toplevel SAS scripts
#'
#' @param sas.path the path to which containing SAS scripts, assuming the SAS scripts under \code{sas.path} are the main scripts
#' @param sas.pattern which pattern to including in analysis as SAS scripts
#' @param output.file the output pdf file which displays the structures
#' @return list of network of toplevel scripts
toplevel.sas.structure <- function(sas.path, sas.pattern='\\.[Ss][Aa][Ss]$', output.file='network.map.pdf') {
    L <- parseSASfolder(sas.path, sas.pattern)
    nn <- createNetwork(L)
    toplevel.scripts <- 
        casefold(sub(sas.pattern,'', basename(list.files(sas.path, pattern=sas.pattern))))
    v.names <- network.vertex.names(nn)
    topind <- match(toplevel.scripts, v.names)
    mat <- nn[,]
    pdf(output.file)
    re <- list()
    for(i in topind) {
        cp <- dfs.matrix.travel(mat, i)
        n1 <- as.network(mat[cp,cp, drop=FALSE])
        vn <- network.vertex.names(n1)
        theCols <- ifelse(v.names[i] == vn,  rgb(242, 30, 19, max = 255), rgb(146, 146, 146, max = 255))
        labelCex <- ifelse(v.names[i] == vn, 1, .9)
        vertexCex <- ifelse(v.names[i] == vn, 1.5, 1)
        plot(n1, main = v.names[i], displaylabels = TRUE, label=vn, vertex.col = theCols, label.cex = rep(labelCex,each=2), vertex.cex=vertexCex)
        re[[ v.names[i] ]] <- n1
    }
    dev.off()
    invisible(re)
}

