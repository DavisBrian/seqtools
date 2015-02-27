#' Check the phenotype data frame
#' 
#' @description This function provides a basic check that the phenotype file is
#'   a suitable for further analysis.  It checks that the required column names
#'   are in the phenotype data frame and that subjects are unique.
#' 
#' @param data an object of type \code{phenotype} or a data frame (or object
#'   coercible by as.data.frame to a data frame) containing the variables in the
#'   model.
#' @inheritParams phenotype
#' 
#' @details  TBD
#' 
#' @return invisible(NULL) 
#'  
#' @export
#
# [TBD] 
#  - add verbose option for debugging purposes
#  - return msgs???
#  - ability to include/exclude "MALE/FEMALE"
#  - lm 'family' (binomial, gaussian, survival) & check the repsonse variable is
#    of the appropriate type
checkPhenotype <- function(data, formula=NULL, id=NULL, gender=NULL, include=NULL, exclude=NULL) {
  
  if (is_phenotype(data)){
    p <- data$data
    formula <- data$formula
    id <- data$id
    gender <- data$gender
    include <- data$include
    exclude <- data$exclude    
  } else {
    p <- data
  }
  
  # check formula
  if (!is.null(formula)){
    formula <- as.formula(formula)    
    
    # check the formula  variables are in the data
    vars <- tryCatch(get_all_vars(formula=formula, data=p), 
                     error = function(err) {
                       stop("One or more formula variables does not exist in the data.\n  ", err)
                     })    
  }
  
  # check id  
  if (!is.null(id)) {
    if (!(id %in% colnames(p))) {
      stop("'", id, "' not a column in the phenotype data frame.")
    }
    if (any(duplicated(p[ , id]))) {
      stop("'", id, "' has duplicated phenotype ids.")
    } else {
      rownames(p) <- p[ , id]
    }
  }
  
  # check gender
  if(!is.null(gender)) {
    if (!(gender %in% colnames(p))) {
      stop("'",gender, "' not found in phenotype data frame")
    }
    gtype <- typeof(p[ , gender])
    g <- unique(p[ , gender])
    if (nlevels(as.factor(g)) > 2L) {
      stop("More than 2 gender levels specified.")
    }
    if(gtype == "integer") {
      if (!all(g %in% c(0, 1, NA))) {
        stop("Gender must be (0/1 or F/T) indicating female/male.")
      }      
    } else if(gtype == "character") {
      if (!all(g %in% c("F", "M", NA))) {
        stop("Gender must be (0/1 or F/T) indicating female/male.")
      }        
    }     
  }
  
  # check that there are move than 1 subject to be included
  if (!is.null(include)) {
    subjects <- intersect(include, rownames(p))
    if (length(subjects) == 0L) {
      stop("No subjects to include.")
    }
  }
  
  # check that the exclude list actually excludes someone
  if (!is.null(exclude)) {
    if (length(intersect(exclude, rownames(p))) == 0L) {
      warning("No subjects to exclude")
    }
    if (length(setdiff(rownames(p), exclude)) == 0L) {
      stop("All subjects excluded.")
    }
  }
  
  # check that the include/exclude lists done have overlapping individuals
  if (!is.null(include) && !is.null(exclude)) {
    subjects <- intersect(include, exclude)
    if (length(subjects) > 0L) {
      stop("Subjects cannot be in both included and exclude lists.")
    }
  }
  
  return(invisible(NULL)) 
}
