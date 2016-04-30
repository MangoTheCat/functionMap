
posix_pardir <- ".."
posix_sep <- "/"
win_pardir <- "\\"

path_isabs <- function(path) {
  if (is_windows()) {
    win_path_isabs(path)
  } else {
    posix_path_isabs(path)
  }
}

#' @importFrom rematch re_match

win_path_isabs <- function(path) {
  device_re <- paste0(
    "^([a-zA-Z]:|",
    "[\\\\\\/]{2}[^\\\\\\/]+[\\\\\\/]+[^\\\\\\/]+)?",
    "([\\\\\\/])?([\\s\\S]*?)$"
  )
  result <- re_match(pattern = device_re, text = path)
  device <- result[, 2]
  isunc  <- device != "" & substr(device, 2, 2) != ":";

  unname(isunc | result[, 3] != "")
}

posix_path_isabs <- function(path) {
  startswith(path, "/")
}

path_rel <- function(path, start = ".") {
  stopifnot(length(start) == 1)
  if (is.na(start)) {
    rep(NA_character_, length(path))
  } else if (is_windows()) {
    win_path_rel(path, start)
  } else {
    posix_path_rel(path, start)
  }
}

posix_path_rel <- function(path, start = ".") {
  if (is.na(start)) {
    rep(NA_character_, length(path))
  } else {
    start <- drop_empty(strsplit(path_abs(start), "/")[[1]])
    vapply(path, posix_path_rel1, "", start = start, USE.NAMES = FALSE)
  }
}

posix_path_rel1 <- function(path1, start) {

  if (is.na(path1)) return(NA_character_)

  path1 <- drop_empty(strsplit(path_abs(path1), "/")[[1]])

  i <- length(common_prefix(path1, start))

  rel_list <- c(
    rep(posix_pardir, length(start) - i),
    if (i == 0) path1 else tail(path1, -i)
  )

  if (length(rel_list) == 0) {
    "."
  } else {
    paste(rel_list, collapse = posix_sep)
  }
}

win_path_rel <- function(path, start = ".") {
  if (is.na(start)) {
    rep(NA_character_, length(path))
  } else {
    start <- path_abs(win_path_norm(start))
    vapply(path, win_path_rel1, "", start = start, USE.NAMES = FALSE)
  }
}

win_path_rel1 <- function(path1, start) {

  if (is.na(path1)) return(NA_character_)

  sstart <- path_split_drive(start)
  start_drive <- sstart[[1]][1]
  start_rest <- sstart[[1]][2]

  path1 <- path_abs(win_path_norm(path1))
  spath1 <- path_split_drive(path1)
  path1_drive <- spath1[[1]][1]
  path1_rest <- spath1[[1]][2]

  if (normcase(start_drive) != normcase(path1_drive)) {
    stop(sprintf(
      "path is on mount %s, start on mount %s",
      path1_drive, start_drive
    ))
  }

  start_list <- drop_empty(strsplit(start_rest, "\\", fixed = TRUE)[[1]])
  path1_list <- drop_empty(strsplit(path1_rest, "\\", fixed = TRUE)[[1]])

  i <- length(common_prefix(normcase(start_list), normcase(path1_list)))

  rel_list <- c(
    rep("..", length(start_list) - i),
    if (i == 0) path1_list else tail(path1_list, -i)
  )

  if (length(rel_list) == 0) {
    "."
  } else {
    paste(rel_list, collapse = "\\")
  }
}

path_abs <- function(path) {
  res <- path

  na  <- is.na(path)
  abs <- path_isabs(path)
  res[!abs & !na] <- file.path(getwd(), path[!abs & !na])

  res[!na] <- path_norm(res[!na])

  res
}

path_norm <- function(path) {
  if (is_windows()) {
    win_path_norm(path)
  } else {
    posix_path_norm(path)
  }
}

win_path_norm <- function(path) {
  vapply(path, win_path_norm1, "", USE.NAMES = FALSE)
}

posix_path_norm <- function(path) {
  vapply(path, posix_path_norm1, "", USE.NAMES = FALSE)
}

posix_path_norm1 <- function(path1) {

  if (path1 == "") return(".")
  if (is.na(path1)) return(NA_character_)

  initial_slashes <- as.integer(startswith(path1, "/"))

  ## POSIX allows one or two initial slashes, but treats three or more
  ## as single slash.
  if (initial_slashes && startswith(path1, "//") &&
      !startswith(path1, "///")) {
    initial_slashes <- 2
  }

  comps <- strsplit(path1, "/")[[1]]
  new_comps <- character()

  for (comp in comps) {

    if (comp == "" || comp == ".") {
      ## nothing to do

    } else if (comp != posix_pardir ||
        (! initial_slashes && length(new_comps) == 0) ||
        (length(new_comps) > 0 && tail(new_comps, 1) == posix_pardir)) {
      new_comps <- c(new_comps, comp)

    } else {
      new_comps <- head(new_comps, -1)
    }
  }

  res <- paste(new_comps, collapse = "/")
  if (initial_slashes) {
    res <- paste0(c(rep("/", initial_slashes), res), collapse = "")
  }

  if (res != "") res else "."
}


win_path_norm1 <- function(path1) {

  ## in the case of paths with these prefixes:
  ## \\.\ -> device names
  ## \\?\ -> literal paths
  ## do not do any normalization, but return the path unchanged
  if (startswith(path1, "\\\\.\\") ||
      startswith(path1, "\\\\?\\")) return(path1)

  path1 <- gsub("/", "\\", path1, fixed = TRUE)
  spl <- path_split_drive1(path1)
  prefix <- spl[1]
  path1 <- spl[2]

  ## Collapse initial backslashes
  if (startswith(path1, "\\")) {
    prefix <- paste0(prefix, "\\")
    path1 <- substr(path1, 2, nchar(path1))
  }

  comps <- as.list(strsplit(path1, "\\", fixed = TRUE)[[1]])

  i <- 1
  while (i <= length(comps)) {

    if (comps[[i]] == "" || comps[[i]] == ".") {
      comps[[i]] <- NULL

    } else if (comps[[i]] == win_pardir) {
      if (i > 1 && comps[[i - 1]] != win_pardir) {
        comps[[i - 1]] <- NULL
        comps[[i - 1]] <- NULL
        i <- i - 1
      } else if (i == 1 && endswith(prefix, "\\")) {
        comps[[i]] <- NULL
      } else {
        i <- i + 1
      }

    } else {
      i <- i + 1
    }
  }

  if (prefix == "" && length(comps) == 0) comps <- "."

  paste0(prefix, paste(unlist(comps), collapse = "\\"))
}

path_split_drive <- function(path) {
  lapply(path, path_split_drive1)
}

path_split_drive1 <- function(path1) {
  npath1 <- gsub("/", "\\", path1, fixed = TRUE)
  if (startswith(npath1, "\\\\") && substr(npath1, 3, 3) != "\\") {
    if (grepl("^\\\\\\\\[^\\\\]+\\\\[^\\\\]+.*$", npath1)) {
      nunc <- sub("^(\\\\\\\\[^\\\\]+\\\\[^\\\\]+).*$", "\\1", npath1)
    } else {
      nunc <- ""
    }
    c(substr(path1, 1, nchar(nunc)),
      substr(path1, nchar(nunc) + 1, nchar(path1)))

  } else if (substr(npath1, 2, 2) == ":") {
    c(substr(path1, 1, 2), substr(path1, 3, nchar(path1)))

  } else {
    c("", path1)
  }
}
