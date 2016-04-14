
match_to_parse_data <- function(calls, func,
                                mode = c("call", "::", "do.call")) {

  mode <- match.arg(mode)

  pd <- attr(func, "src")

  res <- data_frame(
    to = calls,
    type = "call",
    line = NA_integer_,
    col1 = NA_integer_,
    col2 = NA_integer_
  )

  ## No source information
  if (is.null(pd)) return(res)

  for (i in seq_len(nrow(res))) {
    pl <- find_call_in_parse_data(res$to[i], pd, mode)
    if (!is.null(pl)) {
      res$line[i] <- pl$line
      res$col1[i] <- pl$col1
      res$col2[i] <- pl$col2

      pd <- pd[ rownames(pd) != pl$rowname, ]
    }
  }

  res
}

parse_trans_table <- c(
  "<-"   = "LEFT_ASSIGN",
  "->"   = "RIGHT_ASSIGN",
  "="      = "EQ_ASSIGN",
  "[["   = "LBB",
  "for"    = "FOR",
  "if"     = "IF",
  "while"  = "WHILE",
  "next"   = "NEXT",
  "break"  = "BREAK",
  "repeat" = "REPEAT",
  ">"      = "GT",
  ">="     = "GE",
  "<"      = "LT",
  "<="     = "LE",
  "=="     = "EQ",
  "!="     = "NE",
  "&"      = "AND",
  "|"      = "OR",
  "&&"     = "AND2",
  "||"    = "OR2",
  "::"   = "NS_GET",
  ":::"  = "NS_GET_INT"
)

find_call_in_parse_data <- function(call, pd, mode) {

  w <- if (mode == "::") {
    find_call_in_parse_data_pkg(call, pd)

  } else if (mode == "do.call") {
    NA

  } else {
    if (call %in% names(parse_trans_table)) call <- parse_trans_table[call]
    quoted_call <- paste0("'", call, "'")
    which(pd$token == quoted_call |
          pd$token == call |
          pd$token == "SPECIAL" & pd$text == call |
          pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == call)[1]
  }

  if (!is.na(w)) {
    list(
      line = pd$line1[w],
      col1 = pd$col1[w],
      col2 = pd$col2[w],
      rowname = as.character(rownames(pd)[w])
    )
  }
}

find_call_in_parse_data_pkg <- function(call, pd) {

  w <- which(pd$text == call & pd$token == "expr")[1]
  children <- which(pd$parent == pd$id[w])
  child_tokens <- pd$token[children]
  pkgcall_tokens <- c("NS_GET", "SYMBOL_FUNCTION_CALL", "SYMBOL_PACKAGE")

  if (!identical(sort(child_tokens), pkgcall_tokens)) {
    NA

  } else {
    child_text <- pd$text[children]
    rec_call <- paste0(
      child_text[match("SYMBOL_PACKAGE", child_tokens)],
      child_text[match("NS_GET", child_tokens)],
      child_text[match("SYMBOL_FUNCTION_CALL", child_tokens)]
    )

    if (rec_call == call) w else NA
  }
}
