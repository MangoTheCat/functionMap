
find_function_defs <- function(data) {

  ## A function definition with no arguments
  xp_noargs <- paste0(
    "//expr[count(*)=4]",
    "[*[1][self::FUNCTION]]",
    "[*[2][self::OP-LEFT-PAREN]]",
    "[*[3][self::OP-RIGHT-PAREN]]",
    "[*[4][self::expr]]"
  )

  ## A function definition with arguments
  xp_args <- paste0(
    "//expr[count(*)=5]",
    "[*[1][self::FUNCTION]]",
    "[*[2][self::OP-LEFT-PAREN]]",
    "[*[3][self::SYMBOL_FORMALS]]",
    "[*[4][self::OP-RIGHT-PAREN]]",
    "[*[5][self::expr]]"
  )

  xp <- sprintf("(%s) | (%s)", xp_noargs, xp_args)

  fun_exprs <- xml_find_all(data$xml, xp)
  fundefs <- collect_symbols(fun_exprs, text = FALSE)

  ## Try to find function names
  fundefs$var <- find_function_names(fundefs, data)

  ## Add a virtual function, which contains the assignments and calls
  ## outside of any function
  rbind(
    list(var = "_", line1 = 0, col1 = 0, line2 = .Machine$integer.max,
         col2 = 1, start = 0, end = .Machine$integer.max),
    fundefs
  )
}

find_function_names <- function(fundefs, data) {
  if (!is.null(n <- get_top_function_name(fundefs, data))) {
    var <- fundefs$var
    var[1] <- n
    var
  } else {
    fundefs$var
  }
}

get_top_function_name <- function(fundefs, data) {
  if (!nrow(fundefs)) return(NULL)

  xp_fun_left <- paste0(
    "/exprlist/expr[count(*)=3]",
    "[*[1][self::expr][count(*)=1][SYMBOL]]",
    "[*[2][self::LEFT_ASSIGN]]",
    "[*[3][self::expr][FUNCTION]]"
  )

  xp_fun_eq <- paste0(
    "/exprlist[count(*)=3]",
    "[*[1][self::expr][count(*)=1][SYMBOL]]",
    "[*[2][self::EQ_ASSIGN]]",
    "[*[3][self::expr][FUNCTION]]"
  )

  xp_fun <- sprintf("(%s) | (%s)", xp_fun_left, xp_fun_eq)

  node <-  xml_find_first(data$xml, xp_fun)

  symbol <- xml_find_first(node, "expr/SYMBOL")

  xml_text(symbol)
}
