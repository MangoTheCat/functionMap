
#' @importFrom xmlparsedata xml_parse_data
#' @importFrom xml2 read_xml

parse_expression <- function(file = NULL, text = NULL, parsed = NULL) {

  if (is.null(file) + is.null(text) + is.null(parsed) != 2) {
    stop("Supply exactly one of 'file', 'text' and 'parsed'")
  }

  ## We provide both the parse data both in XML and the usual
  ## data frame format. The XML is better for finding complicated
  ## expressions. The data frame is good for finding simple things,
  ## like symbols

  if (is.null(parsed)) {
    parsed <- parse(file = file, text = text, keep.source = TRUE)
    pd <- getParseData(parsed)
  } else {
    pd <- getParseData(parsed)
    if (is.null(pd)) {
      stop("'parsed' has no parse data, use keep.source in parse()")
    }
  }
  xml <- read_xml(xml_parse_data(pd))

  ## Positions, to make it easy to compare what comes first
  maxcol <- max(pd$col1, pd$col2) + 1L
  pd$start <- pd$line1 * maxcol + pd$col1
  pd$end   <- pd$line2 * maxcol + pd$col2

  list(xml = xml, parse = pd, expr = parsed)
}

map_expression <- function(data) {

  assvars  <- find_assignments(data)
  formals  <- find_formals(data)
  funcalls <- find_function_calls(data)
  fundefs  <- find_function_defs(data)

  ## Map assignments to closures, to see what is local, what is global
  fundefs$locals  <- map_assignments(fundefs, assvars, formals)

  ## Now go over the collected data. We process the following events:
  ## 1. Start of closure. We put all assignments that belong to that
  ##    closure, in the "stack".
  ## 2. End of closure. We remove the assignments of the closure from
  ##    the stack.
  ## 3. Function call. Check if the symbol is in the stack. If not, it
  ##    is a "global" call.

  locals <- character()
  res <- integer()

  events <- rbind(
    data_frame(
      what = "start",
      which = seq_len(nrow(fundefs)),
      pos = fundefs$start
    ),
    data_frame(
      what = "end",
      which = seq_len(nrow(fundefs)),
      pos = fundefs$end
    ),
    data_frame(
      what = "call",
      which = seq_len(nrow(funcalls)),
      pos = funcalls$start
    )
  )
  events <- events[order(events$pos), ]

  for (e in seq_len(nrow(events))) {
    ev <- events[e,]
    switch(
      ev$what,
      "start" = { locals <- c(locals, fundefs$local[[ev$which]]) },
      "end" = {
        if (len <- length(fundefs$local[[ev$which]])) {
          locals <- head(locals, -len)
        }
      },
      "call" = {
        if (! funcalls$var[[ev$which]] %in% locals) res <- c(res, ev$which)
      }
    )
  }

  ## We drop the imaginary function definition from the results

  list(
    calls = reset_row_names(funcalls[res, , drop = FALSE]),
    funcs = fundefs
  )
}

#' @importFrom xml2 xml_find_all

xml_assignments <- function(xml) {

  left_xp <- paste0(
    "//expr[count(*)=3]",
    "[*[1][self::expr][count(*)=1][SYMBOL]]",
    "[*[2][self::LEFT_ASSIGN]]",
    "[*[3][self::expr]]"
  )

  right_xp <- paste0(
    "//expr[count(*)=3]",
    "[*[1][self::expr]]",
    "[*[2][self::RIGHT_ASSIGN]]",
    "[*[3][self::expr][count(*)=1][SYMBOL]]"
  )

  eq_xp <- sub("LEFT_ASSIGN", "EQ_ASSIGN", left_xp)

  ass_xp <- sprintf("(%s) | (%s) | (%s)", left_xp, right_xp, eq_xp)

  xml_find_all(xml, ass_xp)
}

#' @importFrom xml2 xml_find_first

find_assignments <- function(data) {
  allass <- xml_assignments(data$xml)
  symbols <- xml_find_first(allass, "expr/SYMBOL")
  collect_symbols(symbols)
}

find_these <- function(parse_data, name) {
  w <- which(parse_data$token == name)
  data.frame(
    stringsAsFactors = FALSE,
    var   = parse_data$text[w],
    line1 = parse_data$line1[w],
    col1  = parse_data$col1[w],
    line2 = parse_data$line2[w],
    col2  = parse_data$col2[w],
    start = parse_data$start[w],
    end   = parse_data$end[w]
  )
}

find_formals <- function(data) {
  find_these(data$parse, "SYMBOL_FORMALS")
}

find_function_calls <- function(data) {
  find_these(data$parse, "SYMBOL_FUNCTION_CALL")
}

#' @importFrom xml2 xml_attr xml_text

collect_symbols <- function(nodes, text = TRUE) {
  data.frame(
    stringsAsFactors = FALSE,
    var   = if (text) xml_text(nodes) else rep(NA_character_, length(nodes)),
    line1 = as.integer(xml_attr(nodes, "line1")),
    col1  = as.integer(xml_attr(nodes, "col1")),
    line2 = as.integer(xml_attr(nodes, "line2")),
    col2  = as.integer(xml_attr(nodes, "col2")),
    start = as.integer(xml_attr(nodes, "start")),
    end   = as.integer(xml_attr(nodes, "end"))
  )
}

map_assignments <- function(funcs, symbols, formals) {

  myfun <- vapply(symbols$start, FUN.VALUE = 1L, function(p) {
    cand <- tail(which(funcs$start <= p & p < funcs$end), 1)
    if (length(cand)) cand else NA_integer_
  })

  fac <- factor(myfun, levels = seq_len(nrow(funcs)))
  res <- tapply(symbols$var, fac, c, simplify = FALSE)

  myfun2 <- vapply(formals$start, FUN.VALUE = 1L, function(p) {
    tail(which(funcs$start <= p & p < funcs$end), 1)
  })

  fac2 <- factor(myfun2, levels = seq_len(nrow(funcs)))
  res2 <- tapply(formals$var, fac2, c, simplify = FALSE)

  mapply(c, res, res2, SIMPLIFY = FALSE)
}
