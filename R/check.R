# use the name passed to x????
check_colnames <- function(x, cn) {
  
  cl <- match.call()
  dfname <- as.character(as.list(cl)$x)
  
  # sanity check on x
  if (!is.data.frame(x) && !is.matrix(x)) {
    stop("Must be a data.frame or a matrix")
  }
  
  if (ncol(x) == 0L) {
    stop("x has 0 columns")
  }
  
  # sanity check on cn
  if (!is.character(cn)) {
    stop("column names must be a character vector")
  }
  
  if (length(cn) == 0L) {
    stop("Must have at least one column name")
  }
  
  if (anyNA(cn)) {
    stop("Column names cannot have missing values")
  }
  
  if (!all(cn %in% colnames(x))) {
    idx <- which(!(cn %in% colnames(x)))
    msg <- paste0(cn[idx], " not a column in the data.frame: ", dfname, collapse="\n")
    stop(msg)
  }
  return(invisible(NULL))
}


# assumes:
# - x is a data.frame
# - cn is a single column name in x
# - type is an valid column type in a data.frame
check_type <- function(x, cn, type) {
  cl <- match.call()
  df_name <- as.character(as.list(cl)$x)
  cn_name <- as.character(as.list(cl)$cn)
  
  # check cn in x
  check_colnames(x, cn)
  
  cn_type <- typeof(x[ , cn])
  cn_length <- length(cn)
  
  stop_msg <- paste0(cn_name, ": ", cn, " is of type ", cn_type, ".  Must be of type ", type)
  
  # if (cn_length == 0 then cn must be NULL or something like character(0)
  if (cn_length == 1L) {
    if (identical(type, "character")) {
      if (!is.character(x[ , cn])) {
        stop(stop_msg)
      }
    } else if (identical(type, "logical")) {
      if (!is.logical(x[ , cn])) {
        stop(stop_msg)
      }
    } else if (identical(type, "integer")) {
      if (!is.integer(x[ , cn])) {
        stop(stop_msg)
      }
    } else if (identical(type, "double") || identical(type, "numeric")) {
      if (!is.double(x[ , cn]) && !is.numeric(x[ , cn])) {
        stop(stop_msg)
      }
    } else if (identical(type, "factor")) {
      if (!is.factor(x[ , cn])) {
        stop(stop_msg)
      }
    } else {
      stop(paste0("type ", type, " not supported."))
    }
  } else if (cn_length >= 2L) {
    stop("cn must have length 1L.")
  }
  
  return(invisible(NULL))  
}
