

#' Find all function calls in an R script
#'
#' @param rfile The .R input file, or a connection to read from.
#' @param include_base Whether to include calls to base functions
#'   in the output.
#' @return Named list of character vectors.
#'   Name is the caller, contents is the callees.

parse_r_script <- function(rfile, include_base = FALSE) {

  ## Get all functions from the script
  funcs <- get_funcs_from_r_script(rfile)

  ## Get their non-local calls
  res <- lapply(funcs, get_global_calls)

  ## Remove base functions, potentially
  if (!include_base) {
    res <- lapply(res, setdiff, ls(asNamespace("base")))
  }

  res
}


#' Extract all functions from an R script
#'
#' Reads the file into a temporary environment, and checks whether
#' the objects in this environment are functions.
#'
#' @param rfile The .R input file.
#' @return Named list of function objects, they also include
#'   the source code, i.e. they have `srcref` attributes.

get_funcs_from_r_script <- function(rfile) {

  ## Load everything into a temporary environment
  tmp_env <- new.env()
  tryCatch(
    source(rfile, local = tmp_env),
    error = function(e) {
      fname <- if (is.character(rfile)) rfile else class(rfile)[1]
      warning(fname, ": ", e$message, call. = FALSE)
    }
  )

  ## Keep the functions
  keep <- Filter(
    function(x) is.function(get(x, envir = tmp_env)),
    ls(tmp_env, all.names = TRUE)
  )

  mget(keep, envir = tmp_env)
}

#' @importFrom codetools findGlobals

get_global_calls <- function(func) {
  c(findGlobals(func, merge = FALSE)$functions, do_call_globals(func))
}

#' convertToCharacter
#'
#' Convert a list names and characters to a character vector.
#'
#' @param L input list.
#' @return character list representing input \code{L}
#' @export
convertToCharacter <- function(L) {
    # as.character(quote(a + b)) -> '+' 'a' 'b', we should use deparse
    if (is.null(L) || length(L)==0) return(character(0))
    sapply(L, function(x) if (is.language(x)) paste(deparse(x), collapse='') else x, USE.NAMES=FALSE)
}
#' analyse.external.call.pattern
#'
#' match \code{.C}, \code{.Fortran} and \code{.Call}
#'
#' A global option \code{add.prefix.for.external.call} will add \code{C_}, \code{FORTRAN_} and \code{External_} prefix or not
#'
#' @param e expression
#' @return list of experssions
#' @examples \dontrun{
#'     analyse.external.call.pattern( quote( .C('classRF') ) )
#'     analyse.external.call.pattern( quote( .C(classRF) ) )
#' }
analyse.external.call.pattern <- function(e){
    if (is.function(e)) return(Recall(body(e)))
    if (is.atomic(e) || is.symbol(e)) return(NULL)
    L <- NULL
    if (is.list(e)) {
        for(i in seq_along(e)) {
            L <- c(L, Recall(e[[i]]))
        }
        return(L)
    }
    if (is.call(e)) {
         if (e[[1]]=='.C' || e[[1]]=='.Fortran' || e[[1]]=='.Call' || e[[1]] == '.External' ) {
         # we can't use match.call for primitive call
         # assume position 1 is always the .NAME of the routine being called
            if (isTRUE(getOption('add.prefix.for.external.call'))) {
                prefix <- switch(as.character(e[[1]]),
                    '.C' = 'C',
                    '.Fortran' = 'FORTRAN',
                    'EXTERNAL')
                L <- c(L, paste(prefix, convertToCharacter(e[[2]]), sep='_'))
            } else {
                L <- c(L, e[[2]])
            }
         } else {
             if (length(e)>1) {
                 for(i in 2:length(e)) {
                     L <- c(L, Recall(e[[i]]))
                 }
             }
         }
    }
    L
}
