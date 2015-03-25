# copied from https://github.com/hadley/devtools/blob/master/R/utils.r
rule <- function(..., pad = "-") {
  if (nargs() == 0) {
    title <- ""
  } else {
    title <- paste0(...)
  }
  width <- getOption("width") - nchar(title) - 1
  message(title, " ", paste(rep(pad, width, collapse = "")))
}


# is_snpinfo
#' @export
is_snpinfo <- function(x) inherits(phenox, "snpinfo")


# is_genotype
#' @export
is_genotype <- function(x) inherits(phenox, "genotype")


is_categorical <- function(x) {
  is.character(x) || is.factor(x) || is.integer(x) || is.logical(x)
}

get_numeric_cols <- function(x) { which(sapply(x, is.numeric)) }
get_categorical_cols <- function(x) { which(sapply(x, is_categorical)) }

`%[[%` <- function(x, .) x[[.]]
