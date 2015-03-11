#' @title Create a snpinfo object
#'   
#' @description This function creates a snpinfo object.
#' 
#' @param si a data frame containing the SNP information.  Must contain fields given in '.snpNames' and '.aggregateBy'. 
#' @param .snpNames The field of si where the SNP identifiers are found. Default is 'Name'
#' @param .aggregateBy The field of si on which the results were aggregated. Default is 'gene'. For single snps which are intended only for single variant analyses, it is reccomended that they have a unique identifier in this field.
#' @param .chr (optional) The field of si where the chromosome identifiers are found. See Details.
#' @param .pos (optional) The field of si where the snp position identifiers are found. See Details.
#' @param .ref (optional) The field of si where the reference allele for the snps is found. See Details.
#' @param .alt (optional) The field of si where the alternate allele for the snps is found. See Details.
#' @param .filterBy (optional) The (typically logical) field in si in which to keep in further analysis. See Details
#' @param .filterFun (optional) a function to apply to multiple '.filterBy' fields via \code{Reduce}. See Details.
#' @param .otherCols (optional) other fields of si to propagate through. See Details.
#' 
#' @details It is HIGHLY recommended that \code{.chr}, \code{.pos}, \code{.ref}, and \code{.alt} be supplied.  For
#' 
#' \code{.chr} and \code{.pos} are used for branching and output in subsequent
#' analysis.  Useful for analysis where the X chromosome is handled differently
#' than other chromosomes.
#' 
#' \code{ref} and \code{alt} are used in calculating various SNP and subject
#' summary statistics in 'genotype' objects.
#' 
#' \code{.filterBy} and \code{.filterFun} are used to create filters, typically 
#' at the meta-analysis stage (e.g., because they are intronic or common). 
#' \code{TRUE} indicates that the SNP should be kept.  If \code{.filterFun} is
#' specified \code{Reduce} will be applied to the columns specified by
#' \code{.filterBy}.  Note \code{Reduce} is called the result must be a logical
#' vector for each entry in \code{si}. If \code{.filterFun} produces
#' \code{NA}'s, these will be changed to \code{FALSE} with a warning.
#' 
#' \code{.otherCols} 
#' 
#' @seealso \code{Reduce}
#'   
#' @return an object of class 'snpinfo'.  This is a list typicaly used to feed
#'   into an analysis function.
#'   
#'   An object of class 'snpinfo' is a list containing at least the following
#'   components:
#'   
#' \itemize{
#'  \item data containing the SNP information
#'  \item snpNames column name containing the names of SNPs
#'  \item aggregateBy column name which results are to be aggregated
#'  \item chrCol column name containing the chromosome a SNP is found at. 
#'  \item posCol column name containing the position in a chromosome a SNP is found at. 
#'  \item refCol column name containing the reference allele.
#'  \item altCol column name containing the alternate allele.   
#' }
#' 
#' It is important to note that return \code{include} and \code{exclude} are taken from data and not from the input parameters. 
#'   
#' @export
#
# [TBD]
#  - ???
snpinfo <- function(si, .snpNames="Name", .aggregateBy="gene", .chr=NULL, .pos=NULL, .ref=NULL, .alt=NULL, .filterBy=NULL, .filterFun=NULL, .otherCols=NULL) {
  
  cn <- c(.snpNames, .aggregateBy, .chr, .pos, .ref, .alt, .filterBy, .otherCols)
  
  # sanity check that the column names we think we passed are actually in si
  check_colnames(si, cn)
  
  # check input parameter data types are correct
  check_type(si, .snpNames, "character")
  check_type(si, .aggregateBy, "character")
  if (!is.null(.chr)) { check_type(si, .chr, "character") }
  if (!is.null(.pos)) { check_type(si, .pos, "integer") }
  if (!is.null(.ref)) { check_type(si, .ref, "character") }
  if (!is.null(.alt)) { check_type(si, .alt, "character") }
  
  # see if we can apply .filterFun to .filterBy and create a logical vector
  if (is.null(.filterBy)) {
    keep <- TRUE
  } else {
    if (length(.filterBy) == 1L) {
      check_type(si, .filterBy, "logical")
      keep <- si[ , .filterBy]
    } else {      
      if (!is.null(.filterFun) && is.function(match.fun(.filterFun))) {
        keep <- Reduce(.filterFun, si[, .filterBy])
        # test the results after .filterFun is applied
        if (!is.logical(keep)) {
          stop(".filterFun must return a logical vector")
        }
        if (length(keep) != nrow(si)) {
          stop(".filterFun must return a logical vector of length nrow(si)")
        }        
      } else {
        stop(".filterFun is not a function.")
      } 
    }
  }
  
  if (anyNA(keep)) {
    warning(".filterBy or .filterFun produced NA.  Converting NA to FALSE.")
    keep[is.na(keep)] <- FALSE
  }
  
  dat <- unique(data.frame(si[, cn, drop=FALSE], .keep=keep))
  
  structure(
    list(data=dat,
         snpNames=.snpNames,
         aggregateBy=.aggregateBy,
         chrCol=.chr,
         posCol=.pos,
         refCol=.ref,
         altCol=.alt
    ),
    class = "snpinfo"
  )
}

#' @export
head.snpinfo<- function(x, n=6L, ...) {
  stopifnot(length(n) == 1L)
  
  list(data=head(x$data, n=n),
       snpNames=x$snpNames,
       aggregateBy=x$aggregateBy,
       chrCol=x$chr,
       posCol=x$pos,
       refCol=x$ref,
       altCol=x$alt
  )
}

#' @export
tail.snpinfo <- function(x, n=6L, ...) {
  stopifnot(length(n) == 1L)
  
  list(data=tail(x$data, n=n),
       snpNames=x$snpNames,
       aggregateBy=x$aggregateBy,
       chrCol=x$chr,
       posCol=x$pos,
       refCol=x$ref,
       altCol=x$alt
  )  
}

