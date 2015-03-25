#' Summarize the phenotype data frame
#' 
#' @description This function summarizes the phenotype data in a phenotype object.
#' 
#' @param p an object of class 'phenotype'. 
#' @param max.levels maximum number of \code{levels}.  See Details
#' 
#' @details  This is an S3 Methdod that calculates basic summary statistics for phenotype data. 
#' 
#' For varaibles taking discrete values (\code{logical}, \code{integer}, 
#' \code{charater}, and \code{factor}) the number of unique values, including 
#' \code{NA}, are determined.  If this is less than or equal to max.levels, then
#' a \code{table} of that varaible is calcualted.  Note: This has no bearing on
#' whether a variable is treated as a categorical varaible in subsequent
#' analysis.
#'   
#' @return an object of class summary_phenotype.  
#' 
#' For numeric variables 
#' \itemize{
#'  \item \code{N} {the number of non-missing data points in the sample.}
#'  \item \code{Nmiss} {the number of missing data points in the sample.}
#'  \item \code{Min} {the sample minima.}
#'  \item \code{Max} {the sameple maxima.}
#'  \item \code{Median} {the sample median.}
#'  \item \code{Mean} {the sample mean.}
#'  \item \code{SD} {the sample standard deviation.}  
#'  \item \code{Q1} {the sample first quantile.}
#'  \item \code{Q3} {the sample third quantile.}
#' }
#' 
#' For (potientially) categorical varaibles
#' \itemize{ 
#'  \item \code{N} {the number of non-missing data points in the sample.}
#'  \item \code{Nmiss} {the number of missing data points in the sample.}
#'  \item \code{Levels} {the number of unique values in the sample.}
#'  \item \code{Datatype} {the (R internal) type or storage mode of the sample.} 
#' }
#' 
#' For (potientially) categorical varaibles with \code{Levels <= max.levels}  a
#' contingency table of counts for each level.
#' 
#' @export
# [TBD]
#  - pretty write to text file
summary.phenotype <- function(p, max.levels=5L) {
  all_data <- if (is.grouped_df(p)) {
    group_summary <- do(p, smry=summarize_phenotype(.))
    nms <- as.character(group_summary[[1]])
    out <- group_summary %[[% "smry"
    setNames(out, nms)
  } else {
    summarize_phenotype(p, max.levels)
  } 
  return(all_data)
}

summarize_phenotype <- function(p, max.levels=5L) {
  structure(
    list(numeric=summary_numeric_cols(p), 
         categorical=summary_categorical_cols(p, max.levels)
    ),         
    class = "summary_phenotype"
  )  
}


#' @export
print.summary_phenotype <- function(x, ...) {
  rule("Numeric Phenotypes")
  if (is.data.frame(x$numeric)) {
    print.data.frame(x$numeric, right=FALSE, row.names=FALSE)   
  } else {
    print("None") 
  }
  rule("Categorical (potentially) Phenotypes")
  if (is.data.frame(x$categorical$sstats)) {
    print.data.frame(x$categorical$sstats, right=FALSE, row.names=FALSE)
    rule("Categorical Counts")
    cnt <- FALSE
    for (i in 1L:length(x$categorical$counts)) {
      if (!is.null(x$categorical$counts[[i]])) {
        names(dimnames(x$categorical$counts[[i]])) <- names(x$categorical$counts)[i]
        print(x$categorical$counts[[i]])
        cnt <- TRUE
      }
    }
    if (!cnt) {
      print("All potential categorical phenotypes have more levels than max.levels.")
    }
  } else {
    print("None") 
  }  
}

# get_numeric_cols <- function(x) { which(sapply(x, function(cx) { is.numeric(cx)})) }
# get_categoricalc_cols <- function(x) { which(sapply(x, function(cx) { is_categorical(cx)})) }

summary_numeric_cols <- function(dat) {
  cols <- get_numeric_cols(dat)
  
  if (length(cols) > 0L) {
    Variable <- colnames(dat)[cols]
    N <- colSums(!is.na(dat[, cols, drop=FALSE]))
    Nmiss <- colSums(is.na(dat[, cols, drop=FALSE]))
    Min <- sapply(dat[, cols, drop=FALSE], min, na.rm=TRUE)
    Q1 <- sapply(dat[, cols, drop=FALSE], quantile, probs=0.25, na.rm=TRUE)
    Median <- sapply(dat[, cols, drop=FALSE], median, na.rm=TRUE)
    Mean <- colMeans(dat[, cols, drop=FALSE], na.rm=TRUE)
    SD <- sapply(dat[, cols, drop=FALSE], sd, na.rm=TRUE)
    Q3 <- sapply(dat[, cols, drop=FALSE], quantile, probs=0.75, na.rm=TRUE)
    Max <- sapply(dat[, cols, drop=FALSE], max, na.rm=TRUE)
    
    data.frame(Variable, N, Nmiss, Min, Q1, Median, Mean, SD, Q3, Max, stringsAsFactors=FALSE)
    
  } else {
    NA
  }
}

summary_categorical_cols <- function(dat, max.levels=5L) {
  cols <- get_categoricalc_cols(dat)
  
  if (length(cols) > 0L) {
    Variable <- colnames(dat)[cols]
    N <- colSums(!is.na(dat[, cols, drop=FALSE]))
    Nmiss <- colSums(is.na(dat[, cols, drop=FALSE]))
    Levels <- sapply(dat[, cols, drop=FALSE], 
                     FUN=function(x) {
                       if (is.factor(x)) {
                         nlevels(x)
                       } else {
                         length(unique(x))
                       }
                     })
    Datatype <- sapply(dat[, cols, drop=FALSE], typeof)
    
    ss <- data.frame(Variable, N, Nmiss, Levels, Datatype, stringsAsFactors=FALSE)
    
    counts <- lapply(dat[, cols, drop=FALSE], FUN=function(x) {
      if (is.factor(x)) {
        lvls <- nlevels(x)
      } else {
        lvls <- length(unique(x))
      }
      if (lvls <= max.levels) {
        tbl <- table(x, useNA="always")
      }
    }
    )
    list(sstats=ss, counts=counts)        
  } else {
    NA
  }
}
