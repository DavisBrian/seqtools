#' Reduces the phenotype data frame
#' 
#' @description This function reduces a data set to only the variables used in 
#' a model removing subjects with missing data.
#' 
#' @inheritParams phenotype
#' 
#' @details  This function reduces a data set to only the variables used in 
#' a model removing subjects with missing data.  Also, it makes the row names 
#' of the resulting data fram the subject identifier.  If the formula is a
#' coxph formula then it assumes the all terms before the first covariate are
#' needed.
#' 
#' @return data frame with only the columns specified in the formula and with
#          the (optional) row names as the subject identifier.
#'  
#' @export
#
# [TBD] 
#  - add verbose option for debugging purposes
#  - add in support for "phenotype" objects
reducePhenotype <- function(p, pformula, id=NULL, gender=NULL) {
  checkPhenotype(p, pformula, idCol=id, genderCol=gender)   
  
  if (!is.null(id)) {
    rownames(p) <- p[ , id]
    p <- p[, -match(id, colnames(p))]
  } 
  
  if (!is.null(gender)) {
    gtype <- typeof(p[ , gender])
    g <- unique(p[ , gender])
    if(gtype == "character") {
      if (all.equal(g, c("F", "M"))) {
        MF <- p[, gender] == "M"
        p[, gender] <- MF
      } else {
        stop("Unable to convert gender.  Gender must be (0/1 or F/T) indicating female/male.")
      }       
    }                                   
  }
  
  cn <- colnames(get_all_vars(as.formula(pformula), p))
  
  if ((!is.null(gender))) {
    cn <- union(cn, gender)    
  }
  
  if (any(!(cn %in% colnames(p)))) {
    msg <- paste("Formula varaibles:", cn[!(cn %in% colnames(p))], "not found in phenotype file.", sep=" ")
    stop(msg)
  } else {
    if (!is.null(cn)) {
      pheno <- na.omit(p[, cn])
    } else {
      pheno <- na.omit(p)
    }    
  }
  
  return(pheno)
}
