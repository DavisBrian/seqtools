#
reduce_data <- function(x, row.include=NULL, row.exclude=NULL, col.include=NULL, col.exclude=NULL) {
  
  # check that the include/exclude lists don't have overlap
  if ((length(row.include) > 0L) && (length(row.exclude) > 0L)) {
    ids <- intersect(row.include, row.exclude)
    if (length(ids) > 0L) {
      stop("Cannot have the same ids in both row.include and row.exclude")
    }
  }
  
  if ((length(col.include) > 0L) && (length(col.exclude) > 0L)) {
    ids <- intersect(col.include, col.exclude)
    if (length(ids) > 0L) {
      stop("Cannot have the same ids in both col.include and col.exclude")
    }
  }  
  
  # include
  if (length(row.include) > 0L) {
    row.inc <- intersect(row.include, rownames(x))
    if (length(row.inc) == 0L) {
      stop("No rows to include.")
    } else {
      x <- x[row.inc, , drop=FALSE]
    }
  } 
  
  if (length(col.include) > 0L) {
    col.inc <- intersect(col.include, colnames(x))
    if (length(col.inc) == 0L) {
      stop("No columns to include.")
    } else {
      x <- x[ , col.inc, drop=FALSE]
    }
  }  
  
  # exclude
  if (length(row.exclude) > 0L) {
    if (length(intersect(row.exclude, rownames(x))) == 0L) {
      warning("No row exclude ids match the rownames of the data")
    }
    row.exc <- setdiff(rownames(x), row.exclude)
    if (length(row.exc) == 0L) {
      stop("All rows excluded.")
    } else {
      x <- x[row.exc, , drop=FALSE]      
    }
  }
  
  if (length(col.exclude) > 0L) {
    if (length(intersect(col.exclude, colnames(x))) == 0L) {
      warning("No column exclude ids match the colnames of the data")
    }
    col.exc <- setdiff(colnames(x), col.exclude)
    if (length(col.exc) == 0L) {
      stop("All columns excluded.")
    } else {
      x <- x[, col.exc, drop=FALSE]      
    }
  }
  
  return(x)  
}

