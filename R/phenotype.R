#' @title Create a phenotype object
#'   
#' @description This function creates a phenotype object.
#' 
#' @param data a data frame (or object coercible by as.data.frame to a data 
#'   frame) containing the variables in the model.
#' @param formula an object of class "formula" (or one that can be coerced to 
#'   that class): a symbolic description of the model to be fitted.The details 
#'   of model specification are given under 'Details' in the "lm" help file.
#' @param id (optional) column name identifier of the unique subjects in
#'   \code{data}.  If given the phenotype sample ids will become the rownames
#'   of \code{data}. If \code{id} is \code{NULL} the suject id's are assumed
#'   to be the rownames of \code{data}.
#' @param gender (optional) column name identifier for the gender in \code{data}
#' @param include (optional) character vector of the subjects in \code{id} to be
#'   included in the analysis.  See Details.
#' @param exclude (optional) character vector of the subjects in \code{id} to be
#'   excluded in the analysis.  See Details.
#' @param reduce logical.  Should the dataset be reduced to only columns used in
#' formula.  See Details
#'   
#' @details \code{data} and \code{formula} are similar to what is needed for
#'   \code{lm}.  If a formula is not specifed, no further analysis will run.
#'   
#'   If the \code{id} is not specified it is assumed that the rownames in
#'   \code{data} are the unique subject identifier.  If \code{id} is specifed
#'   then \code{rownames(data)} will be set to \code{id}.  Thus the subject id's
#'   must be unique and without duplication.
#'   
#'   If the gender column is not specified it will be set to \code{NULL}. 
#'   
#'   Typically either \code{include} or \code{exclude}, or both, is set to
#'   \code{NULL}. It is important to note that return \code{include} and
#'   \code{exclude} are taken from the subjects in \code{data} and not from the input parameters.
#'   [TBD: explain include/exclude]
#'   
#'   If \code{reduce == TRUE} then \code{data} is reduced to a data.frame
#'   containing the variables used in \code{formula} plus \code{gender}. See
#'   get_all_vars for specifics.
#'   
#'  @seealso get_all_vars 
#'   
#' @return an object of class 'phenotype'.  This is a list typicaly used to feed
#'   into an analysis function.
#'   
#'   An object of class 'phenotype' is a list containing at least the following
#'   components:
#'   
#' \itemize{
#'  \item data data frame of the data for analysis 
#'  \item formula formula to be used in further analysis  
#'  \item gender column name containing the gender of the subjects
#'  \item include the subjects in \code{data} that will be included is further analysis.
#'  \item exclude the subjects in \code{data} that will be excluded is further analysis.    
#' }
#' 
#' It is important to note that return \code{include} and \code{exclude} are taken from data and not from the input parameters. 
#'   
#' @export
#
# [TBD]
#  -add genderChar??? something to demote which character is "MALE/FEMALE"
#  -add "family" (gaussian/binomial/survival)
phenotype <- function(data, .formula=NULL, .id=NULL, .gender=NULL, .groupBy=NULL, .include=NULL, .exclude=NULL) {
  
  if(is.data.frame(data)) {
    old_class <- class(data)
    old_attribures <- attributes(data)
  } else {
    stop("data must be a data.frame or a class which extends a data.frame.")
  }
  
  # check colnames
  cnames <- c(all.vars(.formula), .id, .gender, .groupBy)
  if (length(cnames) > 0L) {
    check_colnames(data, cnames)
  }
  
  # check formula
  if (!is.null(.formula)) {
    check_formula(data, .formula)
  }
  
  # check id
  if (is.null(.id)) {
    data$.idCol <- rownames(data)
    id = ".idCol"
  } 
  check_ids(data, .id)  
  subjects.all <- data[ , .id]
  
  # check gender
    
  # include / exclude    

  if (!is.null(.include)) {
    subjects <- intersect(include, data[ , .id])
    data <- data[(data[ , .id] %in% subjects), , drop=FALSE]
  }
  
  # exclude
  if (!is.null(.exclude)) {
    subjects <- setdiff(data[ , .id], .exclude)
    data <- data[(data[ , .id] %in% subjects), , drop=FALSE]
  }
  
  subjects_include <- data[ , .id]
  
  subjects_exclude <- setdiff(subjects.all, subjects_include)
  if (length(subjects_exclude) == 0L) {
    subjects_exclude <- NULL
  }

  # check groupBy
  data <- if (!is.null(.groupBy)) {
    if(!is_categorical(data[ , .groupBy])) {
      stop("groupBy must be a category.")
    }    
    data %>% group_by_(.groupBy)
  } else {
    as_data_frame(data)
  }
  new_class <- class(data)
  
  structure(
    data,
    formula = .formula,
    idCol = .id,
    genderCol = .gender,
    groupBy = .groupBy,
    included = subjects_include,
    excluded = subjects_exclude,
    class = c("phenotype", new_class)
  )
}

#' @rdname phenotype
#' @export
is.phenotype <- function(x) inherits(x, "phenotype")


# Summary functions  ----------------------------------------------------------

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
#' a \code{table} of that variable is calcualted.  Note: This has no bearing on
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
    group_summary <- do(p, smry=summarize_phenotype(., max.levels))
    nms <- as.character(group_summary[[1]])
    out <- group_summary %[[% "smry"
    setNames(out, nms)
  } else {
    summarize_phenotype(p, max.levels)
  } 
  
  attr(all_data, "groupBy") = attr(p, "groupBy")
  return(all_data)
}

#' @rdname summary.phenotype
#' @export
summarize_phenotype <- function(p, max.levels=5L) {
  structure(
    list(numeric=summary_numeric_cols(p), 
         categorical=summary_categorical_cols(p, max.levels)
    ),         
    class = "summary_phenotype"
  )  
}

#' @rdname phenotype
#' @export
is.summary_phenotype <- function(x) inherits(x, "summary_phenotype")



#' @export
print.summary_phenotype <- function(x, ...) {
  rule("Numeric Phenotypes")
  if (is.data.frame(x$numeric)) {
    print.data.frame(x$numeric, right=FALSE, row.names=FALSE)   
  } else {
    cat("None\n") 
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
      cat("All potential categorical phenotypes have more levels than max.levels.\n")
    }
  } else {
    cat("None\n") 
  }  
}

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
  cols <- get_categorical_cols(dat)
  
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


#' @export
write.summary_phenotype <- function(x, file, append=FALSE, ...) {
  tmp.wid = getOption("width") 
  options(width = 10000)  
  sink(file, append=append)
  
  if (is.summary_phenotype(x)) {
    write_summary_phenotype(x)    
  } else if (is.list(x)) {
    nms <- names(x)
    for (i in nms) {
      txt <- paste0("GROUP (", attr(x, "groupBy"), " == ", i, ")")
      rule(txt, pad="=", align="center", type="txt")
      write_summary_phenotype(x[[i]])
      cat("\n")
    }
    
  } else {
    stop("x is not of class summary_phenotype or list of summary_phenotype objexts")
  }
  
  
  sink()    
  options(width = tmp.wid)   
  return(invisible(NULL))    
}


write_summary_phenotype <- function(x) {
  rule("Numeric Phenotypes", type="txt", align="center")
  if (is.data.frame(x$numeric)) {
    print.data.frame(x$numeric, right=FALSE, row.names=FALSE)   
  } else {
    cat("None\n") 
  }
  rule("Categorical (potentially) Phenotypes", type="txt", align="center")
  if (is.data.frame(x$categorical$sstats)) {
    print.data.frame(x$categorical$sstats, right=FALSE, row.names=FALSE)
    rule("Categorical Counts", type="txt", align="center")
    cnt <- FALSE
    for (i in 1L:length(x$categorical$counts)) {
      if (!is.null(x$categorical$counts[[i]])) {
        names(dimnames(x$categorical$counts[[i]])) <- names(x$categorical$counts)[i]
        print(x$categorical$counts[[i]])
        cnt <- TRUE
      }
    }
    if (!cnt) {
      cat("All potential categorical phenotypes have more levels than max.levels.\n")
    }
  } else {
    cat("None\n") 
  }  
  return(invisible(NULL))  
}