#' Reduces the phenotype data frame
#' 
#' @description This function reduces a data set to only the variables used in 
#' a model removing subjects with missing data.
#' 
#' @param p an object of class 'phenotype'.  
#' 
#' @details  This function reduces a data set to only the variables used in a
#'   model removing subjects with missing data.  Also, it makes the row names of
#'   the resulting data fram the subject identifier.  If a gender column is
#'   specified the column identifiying the gender will be retained. 
#'  
#' @return an object of class 'phenotype'.  \code{data} is minimal, \code{include} is set to, \code{exclude} is set to
#'  
#' @export
#
# [TBD] 
reducePhenotype <- function(p) {
  
  if (class(p) != "phenotype") {
    stop("Object must be of class 'phenotype'")
  }
  
  checkPhenotype(p) 
  
  pheno <- p$data
  
  if (!is.null(p$formula)) {
    cn <- colnames(get_all_vars(p$formula, pheno))    
  } else {
    cn <- colnames(pheno)
  }
  
  if (!is.null(p$id)) {
    rownames(pheno) <- pheno[ , p$id]
    cn <- union(cn, p$id)    
  } 
  
  if ((!is.null(p$gender))) {
    cn <- union(cn, p$gender)    
  }
  
  pheno_reduced <- na.omit(pheno[, cn])
 
  if (nrow(pheno_reduced) == 0L) {
    stop("All phenotype data removed")
  }
  
  p$data <- pheno_reduced
  
#   subjects_dropped <- setdiff(rownames(pheno), rownames(pheno_reduced))
#   if (length(subjects_dropped) > 0L) {
#     p$dropped <- subjects_dropped
#   }
  
  return(p)
}
