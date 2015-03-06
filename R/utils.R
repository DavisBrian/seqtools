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
is_phenotype <- function(x) { class(x) == "phenotype" }


# is_snpinfo
#' @export
is_snpinfo <- function(x){ class(x) == "snpinfo" }


