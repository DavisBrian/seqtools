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

# is_phenotype
#' @export
is_phenotype <- function(x) { identical(class(x), "phenotype") }


# is_snpinfo
#' @export
is_snpinfo <- function(x) { identical(class(x), "snpinfo") }


# is_genotype
#' @export
is_genotype <- function(x) { identical(class(x), "genotype") }

is_grouping <- function(x) {
  is.character(x) || is.factor(x) || is.intger(x) || is.logical(x)
}