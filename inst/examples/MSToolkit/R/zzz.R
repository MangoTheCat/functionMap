  
.onAttach <- function(libname, pkgname ){
  # Add paths from ECTD.ini to the environment
  sourceOut <- try(source( file.path( path.package("MSToolkit"), "ECTD.ini" ))$value)
  if (class(sourceOut) != "try-error") assign("externalPaths", sourceOut, envir = .ectdEnv)
  copyright <- readLines(system.file("COPYRIGHT", package = "MSToolkit" ) )
  copyright <- gsub( "\\$version", packageDescription("MSToolkit", fields = "Version"), copyright)
  packageStartupMessage("\n")
  packageStartupMessage( paste(copyright, collapse = "\n") )
  
  if( "RUnit" %in% search() || "RUnit" %in% .packages(all.available = TRUE)) {
	  unitText <- "# Unit Tests: mstoolkitUnitTests( )"
	  packageStartupMessage( paste(unitText, collapse = "\n") )
  }
  if( .checkGridAvailable() ){
	  gridText <- "# Grid execution available: use 'grid' argument in ?analyzeData"
	  packageStartupMessage( paste(gridText, collapse = "\n") )
	  options(max.clusters = 2)
  }
}
