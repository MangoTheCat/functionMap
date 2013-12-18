## unit tests will not be done if RUnit is not available
if(require("RUnit", quietly=TRUE)) {
 
  require("MASS")
  
  ## --- Setup ---
  pkg <- "MSToolkit" # <-- Change to package name!
  if(Sys.getenv("RCMDCHECK") == "FALSE") {
    ## Path to unit tests for standalone running under Makefile (not R CMD check)
    ## PKG/tests/../inst/unitTests
    unitTestPath   <- file.path( getwd(), "..", "inst", "Runit" )
    systemTestPath <- file.path( getwd(), "..", "inst", "systemTest" )
  } else {
    ## Path to unit tests for R CMD check
    ## PKG.Rcheck/tests/../PKG/unitTests
    unitTestPath   <- system.file(package=pkg, "Runit")
    systemTestPath <- system.file(package=pkg, "systemTest")
  }
  
  cat("\nRunning unit and system tests\n")
  print(list(pkg=pkg, getwd=getwd(), pathToUnitTests=c( unitTestPath, systemTestPath )))
  
  library(package=pkg, character.only=TRUE)
  
  ## --- Testing ---
 
  ## Define tests
  testSuite <- defineTestSuite( name=paste(pkg, "unit testing"), dirs=unitTestPath)
  tests <- runTestSuite(testSuite)
  pathReport <- file.path(unitTestPath, "report")     
  printHTMLProtocol(tests, fileName=paste(pathReport, ".html", sep="") )
  cat("Writing report in `", pathReport, ".html`\n", sep ="")

  testSuiteST <- defineTestSuite( name=paste(pkg, "system testing"), dirs=systemTestPath)
  testsST <- runTestSuite(testSuiteST)
  pathReportST <- file.path(systemTestPath, "report")     
  printHTMLProtocol(testsST, fileName=paste(pathReportST, ".html", sep="") )
  cat("Writing report in `", pathReportST, ".html`\n", sep ="")
  
  ## Return stop() to cause R CMD check stop in case of
  ##  - failures i.e. FALSE to unit tests or
  ##  - errors i.e. R errors
  tmp <- getErrors(tests)
  if(tmp$nFail > 0 | tmp$nErr > 0) {
	testlog <- unlist(lapply(tests[[1]]$sourceFileResults, FUN = function(X) sapply(seq_along(X), FUN = function(Y) paste(X[[Y]][["kind"]], " in ", names(X)[Y], ": ", X[[Y]][["msg"]], sep = ""))))
	testlog <- gsub("\n", " ", testlog[!grepl("^success", testlog)])
	testlog <- paste("  - ", testlog, sep = "", collapse = "\n")
    stop(paste("\n\nunit testing failed (#test failures: ", tmp$nFail,
               ", #R errors: ",  tmp$nErr, ")\n", testlog, "\n\n", sep=""))
  }
  
} else {
  warning("cannot run unit tests -- package RUnit is not available")
}
