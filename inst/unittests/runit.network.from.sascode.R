

test.network.from.sascode <- function() {

    sas.path <- system.file('examples','SAScode',package='functionMap')

    net <- network.from.sascode(sas.path)

    checkTrue( setequal( net %v% 'vertex.names', c("fun2", "init", "modelcode", "util1", "util2", "mainanalysis")))
 
    checkTrue( setequal( (net %v% 'vertex.names')[net %v% 'toplevel'] , c("mainanalysis")))
}
