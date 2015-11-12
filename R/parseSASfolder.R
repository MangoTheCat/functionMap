#' parseSASfolder
#'
#' similar to parse_r_folder, but deal with SAS code
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
    sas.files <- list.files(sas.path, pattern=sas.pattern, recursive=TRUE, full.names=TRUE)
    user.macro.list <- sub(sas.pattern,'',casefold(basename(sas.files)))
    L <- list()
    for(fn in sas.files) {
        L[[ casefold(sub(sas.pattern,'',basename(fn))) ]] <- parseSASscript(fn, user.macro.list)
    }
    L
}

#' toplevel.sas.structure
#'
#' Analysis and visualize toplevel SAS scripts
#'
#' @param sas.path the path to which containing SAS scripts, assuming the SAS scripts under \code{sas.path} are the main scripts
#' @param sas.pattern which pattern to including in analysis as SAS scripts
#' @param output.file the output pdf file which displays the structures
#' @return list of network of toplevel scripts
#' @importFrom network network.vertex.names as.network
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
        theCols <- ifelse(v.names[i] == vn,  rgb(242, 30, 19, maxColorValue = 255), rgb(146, 146, 146, maxColorValue = 255))
        labelCex <- ifelse(v.names[i] == vn, 1, .9)
        vertexCex <- ifelse(v.names[i] == vn, 1.5, 1)
        plot(n1, main = v.names[i], displaylabels = TRUE, label=vn, vertex.col = theCols, label.cex = rep(labelCex,each=2), vertex.cex=vertexCex)
        re[[ v.names[i] ]] <- n1
    }
    dev.off()
    invisible(re)
}

