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
#  -add in a groupBy variable for multiple analyses
phenotype <- function(data, formula=NULL, id=NULL, gender=NULL, include=NULL, exclude=NULL, reduce=FALSE) {
  
  checkPhenotype(data=data, formula=formula, id=id, gender=gender, include=include, exclude=exclude)
  
  if (!is.null(id)) {
    rownames(data) <- data[ , id]    
  }
  subjects.all <- rownames(data)
  
  # include 
  if (!is.null(include)) {
    subjects <- intersect(include, rownames(data))
    data <- data[subjects, ]
  }
  
  # exclude
  if (!is.null(exclude)) {
    subjects <- setdiff(rownames(data), exclude)
    data <- data[subjects, ]
  }
  
  if (!is.null(formula)) {
    form <- as.formula(formula)
  } else {
    form <- NULL
  }
  
  subjects_include <- rownames(data)
  
  subjects_exclude <- setdiff(subjects.all, rownames(data))
  if (length(subjects_exclude) == 0L) {
    subjects_exclude <- NULL
  }
  
  structure(
    list(data=data, 
         formula=form, 
         id=id, 
         gender=gender,
         include=subjects_include,
         exclude=subjects_exclude
    ),
    class = "phenotype"
  )
}
