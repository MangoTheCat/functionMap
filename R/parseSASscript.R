
#' parseSASscript
#' 
#' extract lines, used proc, data step and user macro information from SAS script
#'
#' @param sas.script script name
#' @param user.macros.list the lookup table of user defined macros, if \code{sas.script} has used any of \code{user.macros.list}, it will be reported in the output
#' @param output.format if it's \code{list} then return a single character including used proc name and user macro name; or return a data.frame
#' @return a character of user proc and macro if \code{output.format} is \code{list}, or return  a data.frame of script name, number of lines, times of proc and data step used and user macro used
#' @export
#' @examples \dontrun{
#'  setwd('SASScriptToplevel')
#'  parseSASscript('06_Model build.sas', output.format='list')
#'  parseSASscript('03_ CREATE CSV FOR CLASSING.sas', output.format='data.frame')
#'  parseSASscript('Macros/ModelCode.sas')
#' }

parseSASscript <- function(sas.script, 
                           user.macros.list = sub('\\.sas$','',casefold(basename(list.files(pattern='\\.[Ss][Aa][Ss]$',rec=TRUE)))), 
                           output.format = 'list') {
  trim <- function(x) gsub("^\\s+|", "", x)
  theCode <- casefold(trim(scan(sas.script, what = character(), sep="\n", quiet = TRUE)))
  theCode <- theCode [ theCode != "" ]
  theCode <- theCode [ -grep("^/\\*", theCode) ]  # Remove comment lines
  dataLines <- length(grep("^data ", theCode))
  whichCall <- sapply(paste0("%", user.macros.list, "[;(]+"), function(txt, code) length(grep(txt, code)), code= theCode)
  callFuns <- if (sum(whichCall)) paste0(user.macros.list[whichCall > 0], collapse=",") else ""
  procLines <- grep("^proc", theCode)

  procTab <- NULL
  if (length(procLines)>0) { # there are procs!
    procCode <- gsub(";", " ", substring(theCode [ procLines ], 6))
    procCode <- substring(procCode, 1, regexpr(" ", procCode)-1)
#   assign("allProcs", c(get("allProcs", pos = 1), procCode), pos = 1)
    procTab <- table(procCode)
    procString <- paste(names(procTab), "(", procTab, ")", sep="", collapse=", ")
  } else {
      procString <- ""
  }

  if (output.format=='list') {
      if (!is.null(procTab)) {
          re <- sprintf('[proc]%s', names(procTab))
      } else {
          re <- character(0)
      }
      re <- c(re, user.macros.list[whichCall > 0])
  } else {
      re <- data.frame(Script = gsub(".sas", "", basename(sas.script)), 
        nLines = length(theCode), 
        Procs = procString, 
        DataSteps = dataLines, 
        Calls = callFuns)
  }
  re 
}

