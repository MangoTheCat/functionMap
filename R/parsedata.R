
match_to_parse_data <- function(calls, func, row) {
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
    pl <- find_call_in_parse_data(res$to[i], pd)
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

find_call_in_parse_data <- function(call, pd) {

  if (call %in% names(parse_trans_table)) call <- parse_trans_table[call]
  quoted_call <- paste0("'", call, "'")

  w <- which(pd$token == quoted_call |
             pd$token == call |
             pd$token == "SPECIAL" & pd$text == call |
             pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == call)[1]

  if (!is.na(w)) {
    list(
      line = pd$line1[w],
      col1 = pd$col1[w],
      col2 = pd$col2[w],
      rowname = as.character(rownames(pd)[w])
    )
  }
}
