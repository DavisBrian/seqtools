#' Summarize the phenotype data frame
#' 
#' @description This function summarizes the phenotype data in a phenotype object.
#' 
#' @param p an object of class 'phenotype'. 
#' @param max.levels maximum number of \code{levels}.  See Details
#' 
#' @details  This function calcualtes basic summary statistics for phenotype data. [TBD: table of summary statsitcs based on data type].
#' 
#' If the number of unique character, integer or factor levels is less than or equal to max.levels 
#'  
#' @return an object of class 'phenotype'.  \code{data} is minimal, \code{include} is set to, \code{exclude} is set to
#'  
#' @export
#
# [TBD] 
summary.phenotype <- function(p, max.levels=5) {
  if (class(p) != "phenotype") {
    stop("Object must be of class 'phenotype'")
  }
  checkPhenotype(p) 
  pheno <- p$data
  
  # numeric summary statsistics
  cols <- sapply(pheno, function(x) { (is.numeric(x) | is.integer(x))})
  if (sum(cols) > 0L) {
    Variable <- colnames(pheno)[cols]
    N <- colSums(!is.na(pheno[, cols, drop=FALSE]))
    Nmiss <- colSums(is.na(pheno[, cols, drop=FALSE]))
    Min <- sapply(pheno[, cols, drop=FALSE], min, na.rm=TRUE)
    Q1 <- sapply(pheno[, cols, drop=FALSE], quantile, probs=0.25, na.rm=TRUE)
    Median <- sapply(pheno[, cols, drop=FALSE], median, na.rm=TRUE)
    Mean <- colMeans(pheno[, cols, drop=FALSE], na.rm=TRUE)
    SD <- sapply(pheno[, cols, drop=FALSE], sd, na.rm=TRUE)
    Q3 <- sapply(pheno[, cols, drop=FALSE], quantile, probs=0.75, na.rm=TRUE)
    Max <- sapply(pheno[, cols, drop=FALSE], max, na.rm=TRUE)
    
    summary_numeric <- data.frame(Variable, N, Nmiss, Min, Q1, Median, Mean, SD, Q3, Max, stringsAsFactors=FALSE)
    
  } else {
    summary_numeric <- NA
  }
  
  
  # factor
  cols <- sapply(pheno, function(x) { (!is.numeric(x) | is.integer(x))})
  if (sum(cols) > 0L) {   
    Variable <- colnames(pheno)[cols]
    N <- colSums(!is.na(pheno[, cols, drop=FALSE]))
    Nmiss <- colSums(is.na(pheno[, cols, drop=FALSE]))
    Levels <- sapply(pheno[, cols, drop=FALSE], 
                     FUN=function(x) {
                       if (is.factor(x)) {
                         nlevels(x)
                       } else {
                         length(unique(x))
                       }
                     })
    Datatype <- sapply(pheno[, cols, drop=FALSE], typeof)
    
    ss <- data.frame(Variable, N, Nmiss, Levels, Datatype, stringsAsFactors=FALSE)
    
    counts <- lapply(pheno[, cols, drop=FALSE], FUN=function(x) {
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
    summary_cat <- list(sstats=ss, counts=counts)
  } else {
    summary_cat <- NA
  }
  
  structure(
    list(
      numeric=summary_numeric, 
      categorical=summary_cat
    ), 
    class = "summary_phenotype"
  )
}

#' @export
print.summary_phenotype <- function(x, ...) {
  rule("Numeric Phenotypes")
  if (is.data.frame(x$numeric)) {
    print(x$numeric, right=FALSE, row.names=FALSE)   
  } else {
    print("None") 
  }
  rule("Categorical (potentially) Phenotypes")
  if (is.data.frame(x$categorical$sstats)) {
    print(x$categorical$sstats, right=FALSE, row.names=FALSE)
    rule("Categorical Counts")
    for (i in 1L:length(x$categorical$counts)) {
      if (!is.null(x$categorical$counts[[i]])) {
        names(dimnames(x$categorical$counts[[i]])) <- names(x$categorical$counts)[i]
        print(x$categorical$counts[[i]])
      }
    }
  } else {
    print("None") 
  }  
}