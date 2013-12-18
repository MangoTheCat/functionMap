"parsePredCode" <- function(
		model, 
		respCol = getEctdColName("Response")
)
{
	###############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# parsePredCode.R 23NOV09
	#
	# Author: Rich
	###############################################################################
	# DESCRIPTION: Converts NONMEM Modelling code to executable S code, returning
	# either a list of commands or a vector of commands
	###############################################################################

	# Basic parsing
	model <- .stripBlanks(model)				# Remove leading and trailing spaces from the model vector
	model <- .stripComments(model)  			# Strip comments
	model <- .stripExitAbort(model)				# Remove EXIT and ABORT statments
	model <- gsub(" *\\(", "(", model)			# Replace space bracket with bracket
	model <- gsub("=", " = ", model)			# Replace space bracket with bracket
	model <- model[model != ""]					# Remove empty elements
	model <- .parseIfElseStatements(model)		# If/Else statements
	model <- .stripBlanks(model)				# Remove leading and trailing spaces from the model vector
	model <- .convertNmFunctions (model)		# EXP( -> exp(, ...
	model <- .convertNmArithmetic(model)		# ** -> ^, ...
	model <- .convertNmOperators (model)		# .AND. -> & ...
	model <- .convertNmParameters(model) 		# THETA(1) -> TH1 ...
	model <- model[ grep("<-", model) ]			# Only include
	model <- .stripBlanks(model)				# Remove leading and trailing spaces from the model vector
	model <- .changeVarNames(model, respCol)	# F -> FX, etc
	model
}

.stripExitAbort <- function(txt) {
	if (length(xRows <- grep("EXIT", txt))) txt <- txt[ - xRows]
	if (length(xRows <- grep("ABORT", txt))) txt <- txt[ - xRows]
	txt
}

.convertNmFunctions <- function(model)
{
	lowerThese <- c("log10", "log", "exp", "sqrt", "abs", "asin", "acos", "atan", "sin", "cos", "tan")
	for (i in lowerThese) {
		findMe <- paste(casefold(i, upper = TRUE), "(", sep="\\")
		replaceMe <- paste(i, "( ", sep="")
		model <- gsub(findMe, replaceMe, model)
	}
	model
}

## MKS - 24Jun11
## Changed second arguments below to include decimal place 
## to handle cases where operator is numeric but non integer.
## Following Klaas Prins suggestion

.convertNmOperators <- function(model) {
	model <- gsub(" = ", " <- ", model)
	model <- gsub("([A-Za-z0-9]+)\\.EQ\\.([A-Za-z0-9]*\\.?[0-9]+)", "(\\1 == \\2)", model)
	model <- gsub("([A-Za-z0-9]+)\\.NE\\.([A-Za-z0-9]*\\.?[0-9]+)", "(\\1 != \\2)", model)
	model <- gsub("([A-Za-z0-9]+)\\.NQ\\.([A-Za-z0-9]*\\.?[0-9]+)", "(\\1 != \\2)", model)
	model <- gsub("([A-Za-z0-9]+)\\.LT\\.([A-Za-z0-9]*\\.?[0-9]+)", "(\\1 < \\2)", model)
	model <- gsub("([A-Za-z0-9]+)\\.GT\\.([A-Za-z0-9]*\\.?[0-9]+)", "(\\1 > \\2)", model)
	model <- gsub("([A-Za-z0-9]+)\\.GE\\.([A-Za-z0-9]*\\.?[0-9]+)", "(\\1 >= \\2)", model)
	model <- gsub("([A-Za-z0-9]+)\\.LE\\.([A-Za-z0-9]*\\.?[0-9]+)", "(\\1 <= \\2)", model)
	model <- gsub("\\.OR\\.", " | ", model)
	model <- gsub("\\.AND\\.", " & ", model)
	model
}

.convertNmArithmetic <- function(model) {
	model <- gsub("\\*\\*", " ^ ", model)
	model <- gsub("\\*", " * ", model)
	model <- gsub("/", " / ", model)
	model <- gsub("-", " - ", model)
	model <- gsub("\\+", " + ", model)
	model
}

.convertNmParameters <- function(model) {
	# Search Strings
	thetaFind <- "THETA\\(([0-9]+)\\)"; thetaReplace <- "TH\\1"
	etaFind <- "ETA\\(([0-9]+)\\)"; etaReplace <- "ETA\\1"
	epsFind <- "EPS\\(([0-9]+)\\)"; epsReplace <- "EPS\\1"
	model <- gsub(thetaFind, thetaReplace, model)
	model <- gsub(etaFind, etaReplace, model)
	model <- gsub(epsFind, epsReplace, model)
	model
}

.stripBlanks <- function(txt) {
	txt <- gsub("^ *", "", txt)
	txt <- gsub(" *$", "", txt)
	txt
}

.stripComments <- function(txt) {
	if (length(xRows <- grep("^ *[\";]", txt) )) txt <- txt[ - xRows ]	# Remove Fortran coding
	txt <- gsub(";.*$", "", txt)
	txt
}

.applyIfStatement <- function( txt, ifClause, notIf = FALSE) {
	splitText <- strsplit(txt, "=")[[1]]
	if (length(splitText) != 2) ectdStop("Problem with parsing target from 'IF' clause")
	theAnswer <- .stripBlanks(splitText[2])
	if (notIf) ifClause <- paste("!", ifClause, sep="")
	if (length(grep("[a-zA-Z]+", theAnswer))) theAnswer <- paste("(", theAnswer, ") [", ifClause, "]")
	paste(.stripBlanks(splitText[1]), "[", ifClause, "] =", theAnswer)	
}

.getIfClause <- function(txt) {
	if (length(txt) != 1) ectdStop("Multiple statements passed to 'IF statement' extraction function")
	firstOpen <- gregexpr("\\(", txt)[[1]]
	lastClose <- gregexpr("\\)", txt)[[1]]
	if (!length(firstOpen) | !length(lastClose)) ectdStop("Brackets not matching in 'IF' statement")
	ifStatement <- .stripBlanks(substring(txt, min(firstOpen) + 1, max(lastClose) - 1))
	if (!length(ifStatement)) ectdStop("Could not extract clause from single 'IF' statement")
	ifStatement
}


.parseIfElseStatements <- function(txt) {
	
	# Deal with stray THENs
	strayThen <- grep("^ *THEN *$", txt)
	if (length(strayThen)) {
		txt [ strayThen - 1 ] <- paste(txt [ strayThen - 1 ], "THEN")
		txt <- txt [ -strayThen ]
	}
	
	# Deal with single IF statements first
	isThen <- grep(" THEN *$", txt)
	isElseIf <- grep("ELSEIF\\(", txt)
	isIf <- setdiff(grep("IF\\(", txt), isElseIf)
	singleTest <- length(isIf) && length(singleIf <- setdiff(isIf, c(isThen, isElseIf)))
	if (singleTest) {
		txt[singleIf] <- sapply(txt[singleIf] , .convertSingleIf )
	}
	
	# Now deal with multiple IF/ELSE statements
	isThen <- grep(" THEN *$", txt)
	isElseIf <- grep("ELSEIF\\(", txt)
	isIf <- setdiff(grep("IF\\(", txt), isElseIf)
	isEndIf <- grep("ENDIF", txt)
	isElse <- grep("ELSE", txt)
	if (!length(isEndIf)) return(txt)
	
	# Check we have correct structure (test THEN placement, and IF/ENDIF matching)
	testIfs <- sort(c(isIf, isElseIf))
	testThens <- sort(isThen)
	isThenTest <- length(testIfs) == length(testThens) && all(testIfs == testThens)
	if (!isThenTest) ectdStop("The placement of 'THEN' statements is not correct in 'IF/ELSE' block")
	if (length(isIf) != length(isEndIf)) ectdStop("'IF' and 'ENDIF' not matching")
	
	# Split into IF/ENDIF blocks
	nBlocks <- length(isIf)
	for (i in nBlocks:1) {
		getBlock <- txt[isIf[i]:isEndIf[i]]
		replaceBlock <- .parseIfElseBlock(.stripBlanks(getBlock))
		if (length(replaceBlock)) {
			txt <- replace(txt, isIf[i]:isEndIf[i], "")
			txt <- append(txt, replaceBlock, isIf[i])
			txt <- txt [ txt != "" ]
		}
	}
	
	# Return updated code
	txt
}


.parseIfElseBlock <- function(txt) {
	
	# Check input
	if (txt[length(txt)] != "ENDIF") ectdStop("Incorrect element passed to IfElse block parser")
	if (!length(grep("^IF\\(", txt[1]))) ectdStop("Incorrect element passed to IfElse block parser")
	
	# Find blocks
	isElseIf <- grep("ELSEIF\\(", txt)
	isIf <- setdiff(grep("IF\\(", txt), isElseIf)
	isElse <- setdiff(grep("ELSE", txt), isElseIf)
	isEndIf <- grep("ENDIF", txt)
	if (length(isElse) != 1 | length(isEndIf) != 1) ectdStop("Incorrect format in IF/ELSE block'")
	
	if (length(grep("ELSEIF\\(", txt))) {
		
		whereClauses <- sort(c(isIf, isElseIf))
		getClauses <- sapply(txt[whereClauses], .getIfClause)
		breakStatements <- c(isIf, isElseIf, isElse, isEndIf)
		breakGroups <- cumsum((1:length(txt)) %in% breakStatements)
		splitCommands <- split( txt [ - breakStatements ], breakGroups [ - breakStatements ] )
		if (length(splitCommands) != length(getClauses) + 1) ectdStop("Could not match nested ifelse statements")
		
		# Extend if/else clauses
		addClause <- paste("!", getClauses, "", sep="", collapse=".AND.")
		for (i in length(getClauses):2) {
			getClauses[i] <- paste( paste("!", getClauses[1:(i-1)], "", sep="", collapse=".AND."), getClauses[i], sep=".AND.")
		}
		getClauses <- c(getClauses, addClause)
		if (length(splitCommands) != length(getClauses) ) ectdStop("Could not match nested ifelse statements")
		
		# Put together final statements
		outText <- c()
		for (i in 1:length(splitCommands)) {
			outText <- c(outText, sapply(splitCommands[[i]], .applyIfStatement, ifClause = getClauses[i]))
		}
	}
	else {
		ifClause <- .getIfClause(txt[1])
		isStatements <- .stripBlanks(txt[2:(isElse-1)])
		notStatements <- .stripBlanks(txt[(isElse+1):(isEndIf-1)])
		isStatements <- sapply(isStatements, .applyIfStatement, ifClause = ifClause)
		notStatements <- sapply(notStatements, .applyIfStatement, ifClause = ifClause, notIf = TRUE)
		outText <- as.vector(c(isStatements, notStatements))
	}
	outText
}

.changeVarNames <- function(txt, respCol = getEctdColName("Response")) {
	
	# Change the "F" variable
	findF <- grep("^F +<-", txt)
	if (length(findF) && findF[1] != length(txt)) {
		txt[findF] <- paste("X", txt[findF], sep= "")
		targetRange <- findF[1]:length(txt)
		txt[targetRange] <- gsub(" +F +", " XF ", txt[targetRange])
		txt[targetRange] <- gsub("^F +", "XF ", txt[targetRange])
		txt[targetRange] <- gsub(" +F$", " XF", txt[targetRange])
	}
	
	# Add "Y > Response" statement
	findY <- grep("^Y +<-", txt)
	if (length(findY)) txt <- c(txt, paste(respCol, "<- Y  # Additional Command Added"))	
	
	txt
}

.convertSingleIf <- function(txt) {
	if (length(txt) > 1) ectdStop("More than 1 statement passed into a 'single IF' parsing routine")
	txt <- .stripBlanks(txt)
	if (!length(grep("^IF", txt))) ectdStop("Invalid single 'IF' statement")
	if (!length(grep("=", txt))) ectdStop("No assignment in single 'IF' statement")
	if (length(grep("THEN", txt))) ectdStop("'THEN' exists in single 'IF' statement")
	
	# Get IF clause
	firstOpen <- gregexpr("\\(", txt)[[1]]
	lastClose <- gregexpr("\\)", txt)[[1]]
	if (!length(firstOpen) | !length(lastClose)) ectdStop("Brackets not matching in 'IF' statement")
	ifStatement <- .stripBlanks(substring(txt, min(firstOpen) + 1, max(lastClose) - 1))
	if (!length(ifStatement)) ectdStop("Could not extract clause from single 'IF' statement")
	
	# Parse the rest
	.applyIfStatement(substring(txt, lastClose+1), ifStatement)
}
