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

print_rule <- function(..., pad = "-") {
  if (nargs() == 0) {
    title <- ""
  } else {
    title <- paste0(...)
  }
  width <- 80 - nchar(title) - 1
  cat(paste0(title, " ", paste(rep(pad, width), collapse = "")), "\n")
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

#' @export
write.Robj <- function(x, filename) {
  tmp.wid = getOption("width")  # save current width
  options(width = 10000)        # increase output width
  sink(filename)                # redirect output to file
  print(x)                      # print the object
  sink()                        # cancel redirection
  options(width = tmp.wid)      # restore linewidth
  return(invisible(NULL))       # return (nothing) from function
} 
