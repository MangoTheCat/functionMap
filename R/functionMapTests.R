
##' Run package regression tests
##'
##' Executes the package regression tests, and produces a html report. The test suite is run with the RUnit package.
##' @title Run package regression tests
##' @param unitTestPath Path where the scripts are located.  By default, will use the package installation directory.
##' @param printTestProtocol Logical flag.  Should HTML reports be produced?
##' @param printTestFile The name of the HTML reports.
##' @return The results of executing the test suites via the function \code{runTestSuite}. 
##' @author Mango Solutions
##' @keywords debugging
##' @examples \dontrun{
##' x <- functionMapTests(printTestProtocol = FALSE)
##' summary(x)
##' }
##' @export

functionMapTests <- function(unitTestPath = system.file(package="functionMap", "unittests"), 
		printTestProtocol = FALSE, printTestFile = "reportingunit.html")
{
	stopifnot(require("RUnit", quietly = TRUE))
	
	TestPath <- normalizePath(unitTestPath)
	if (!exists(".functionMapTestEnv", envir = .GlobalEnv)) assign(".functionMapTestEnv", new.env(), envir = .GlobalEnv)
	assign("TestPath", TestPath, envir = .functionMapTestEnv)
	
	testSuite <- defineTestSuite("survivalToolkit unit test suite", dirs = unitTestPath, testFileRegexp = "^runit\\..+\\.[rR]$")
	res <- runTestSuite(testSuite)
	if(printTestProtocol) printHTMLProtocol(res, fileName = printTestFile )
	if (exists(".functionMapTestEnv", envir = .GlobalEnv)) rm(.functionMapTestEnv, envir = .GlobalEnv)
	res
}



