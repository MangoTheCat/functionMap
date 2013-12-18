writeLogFile <- function(
  jobStatuses,           # A vector that indicates the status of the jobs that are currently running
  startingTime,        # The starting time of the jobs
  logFileName = "jobstatus.log",
  statusNames = c("PEND", "RUN", "DONE", "EXIT", "SUSPEND", "UNKWN"),
  workingPath = getwd()
) 
{
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # writeLogFile.R, July 13
  #
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Writes a log file of jobs currently running
  # KEYWORDS: Documented in Pfizer ECTD Project Support Functions Design Specification
  ###############################################################################

  # Create path to log file
  fullLogFile <- file.path(workingPath, logFileName)
  
  # Perform basic input checks
  if(length(jobStatuses) == 0) ectdStop("Input vector is empty")
  
  # Check for a single starting time
  if (length(startingTime) != 1) ectdStop("Must supply a single starting time")
  
  # Group suspend status to "SUSPEND" item
  suspendTest <- jobStatuses %in% c("PSUSP", "USUSP", "SSUSP", "ZOMBI")
  if (any(suspendTest)) jobStatuses[suspendTest] <- "SUSPEND"
  
  # Group "wait" status into "pending"
  waitTest <- jobStatuses == "WAIT" 
  if (any(waitTest)) jobStatuses[waitTest] <- "PEND"
  
  # Check values passed into job status vector
  if (!all(jobStatuses %in% statusNames)) { 
    missingStatuses <- paste(jobStatuses[jobStatuses%!in%statusNames], collapse=", ")
    ectdStop(paste("Input vector contains unknown status values", missingStatuses, sep=" "))
  }

  # Check to see if all jobs are done.
  if(all(jobStatuses %in% c("DONE", "EXIT")))  {
    nDone <- sum(jobStatuses == "DONE")
    nFail <- sum(jobStatuses == "EXIT")
    cat("\nAll jobs completed\n\nSuccessful runs:", nDone, "\nUnsuccessful runs:", nFail, "\n", file=fullLogFile)
    return(invisible())
  }    
  
  # Not all jobs done, print log                                                            
  cat("\nSome grid jobs not yet completed", file=fullLogFile)
  diffTime <- Sys.time() - startingTime
  timeString <- paste("\nTime since the start of execution:", diffTime, attr(diffTime, "units"), "\n")
  cat(timeString, file = fullLogFile, append = TRUE)

  statusTable <- table(jobStatuses)
  correctOrder <- order(match(names(statusTable), statusNames))
  statusTable <- statusTable[correctOrder]
  
  # Compute the percentages for each status and format them 
  statusText <- sprintf("%15s : %5d (%3d%%)", names(statusTable), statusTable, round(100 * statusTable / sum(statusTable) ))

  # Write out the file
  cat(statusText, file=fullLogFile, append=TRUE, sep = "\n")

  invisible()
}