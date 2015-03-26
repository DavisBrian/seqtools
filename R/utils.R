# copied from https://github.com/hadley/devtools/blob/master/R/utils.r
rule <- function(..., pad = "-", align="left", type="message") {
#  tmp.wid = getOption("width")  
  
  if (nargs() == 0) {
    title <- ""
  } else {
    title <- paste0(...)
  }
  
  len <- if(identical("message", type)) {
    getOption("width")
  } else {
    80L
  }
  
  out <- if (identical("left", align)) {
    width <- len - nchar(title) - 1L
    pad_line <- paste(rep(pad, width), collapse = "")
    paste0(title, " ", pad_line)     
  } else if (identical("right", align)) {
    width <- len - nchar(title) - 1L
    pad_line <- paste(rep(pad, width), collapse = "")
    paste0(pad_line, " ", title)      
  } else if (identical("center", align)) {
    width <- floor((len - nchar(title) - 2L)/2)
    pad_line <- paste(rep(pad, width), collapse = "")
    paste0(pad_line, " ", title, " ", pad_line)  
  } else {
    stop("align must be 'left', 'right', or 'center'.")
  }
  
  if(identical("message", type)) {
    message(out)
  } else {
    cat(paste0(out, "\n"))
  }
  
#  options(width = tmp.wid) 
  return(invisible(NULL))  
}

# print_rule <- function(..., pad = "-") {
#   if (nargs() == 0) {
#     title <- ""
#   } else {
#     title <- paste0(...)
#   }
#   width <- 80 - nchar(title) - 1
#   cat(paste0(title, " ", paste(rep(pad, width), collapse = "")), "\n")
# }


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
