
test.parseSASfolder <- function() {

    sas.path <- system.file('examples','SAScode',package='functionMap')

    L <- parseSASfolder(sas.path)

    checkTrue( setequal( names(L), c("fun2", "init", "modelcode", "util1", "util2", "mainanalysis")))
}

test.parseSASscript <- function() {
# We can return data.frame

    sas.path <- system.file('examples','SAScode',package='functionMap')
    L <- parseSASfolder(sas.path)
    user.macros <- names(L)

    x <- parseSASscript(file.path(sas.path, 'MainAnalysis.SAS'), user.macros, output.format='data.frame')
    
    checkEquals( x, structure(list(Script = structure(1L, .Label = "MainAnalysis.SAS", class = "factor"), 
    nLines = 2L, Procs = structure(1L, .Label = "", class = "factor"), 
    DataSteps = 0L, Calls = structure(1L, .Label = "init,modelcode", class = "factor")), .Names = c("Script", 
"nLines", "Procs", "DataSteps", "Calls"), row.names = c(NA, -1L
), class = "data.frame") )

}
