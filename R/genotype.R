#' @title Create a genotype object
#'   
#' @description This function creates a genotype object.
#' 
#' @param Z A numeric genotype matrix (dosage matrix) or coercible with 
#'   as.matrix - rows correspond to individuals and columns correspond to SNPs. 
#'   Use 'NA' for missing values. The column names of this matrix should 
#'   correspond to SNP names in the \code{snpinfo} object. The row names of this
#'   matrix should correspond to the id in the \code{phenotype} object.
#' @param subject.include (optional) character vector of the subjects in
#'   \code{rownames(Z)} to be included in the analysis.  See Details.
#' @param subject.exclude (optional) character vector of the subjects in
#'   \code{rownames(Z)} to be excluded in the analysis.  See Details.
#' @param snp.include (optional) character vector of the SNPs in
#'   \code{colnames(Z)} to be included in the analysis.  See Details.
#' @param snp.exclude (optional) character vector of the SNPs in
#'   \code{colnames(Z)} to be excluded in the analysis.  See Details.
#'   
#' @details Z should be a numeric matrix
#' 
#'   Typically either \code{subject.include} or \code{subject.exclude}, or both,
#'   is set to \code{NULL}. It is important to note that return \code{include}
#'   and \code{exclude} are taken from the subjects in \code{Z} and not from the
#'   input parameters.
#'   
#'   Typically either \code{snp.include} or \code{snp.exclude}, or both, is set
#'   to \code{NULL}. It is important to note that return \code{include} and 
#'   \code{exclude} are taken from the snps in \code{Z} and not from the input
#'   parameters.
#'   
#' @return an object of class 'genotype'.  
#'
#'   An object of class 'genotype' is a list containing at least the following
#'   components:
#'   
#' \itemize{
#'  \item data data frame of the data for analysis 
#'  \item AAC alternate allele count.
#'  \item AAF alternate allele frequency. 
#'  \item gender column name containing the gender of the subjects
#'  \item include the subjects in \code{data} that will be included is further analysis.
#'  \item exclude the subjects in \code{data} that will be excluded is further analysis.    
#' }
#' 
#' It is important to note that return \code{include} and \code{exclude} are taken from data and not from the input parameters. 

#' @export
#
# [TBD]
#  - sparse matrix
genotype <- function(Z, subject.include=NULL, subject.exclude=NULL, snp.include=NULL, snp.exclude=NULL) {

  if (!is.matrix(Z)) {
    Z <- as.matrix(Z)
  }
  
  # make sure the data is numeric
  if (!is.numeric(Z)) {
    stop("Genotype matrix is ", typeof(Z), " must be numeric (integer or double).")    
  }
  
  subjects_all <- rownames(Z)
  snps_all <- colnames(Z)
  
  data <- reduce_data(Z,
                      row.include=subject.include,
                      row.exclude=subject.exclude,
                      col.include=snp.include,
                      col.exclude=snp.exclude
                      )
    
  structure(
    list(data=data,
         AAC=colSums(data, na.rm=TRUE),
         AAF=colMeans(data, na.rm=TRUE)/2.0,
         included=list(subjects=rownames(data), snps=colnames(data)),
         excluded=list(subjects=setdiff(rownames(Z), rownames(data)), 
                       snps=setdiff(colnames(Z), colnames(data)))
    ),
    class = "genotype"
  )    
}


#' @export
head.genotype <- function(x, n=6L, ...) {    
  stopifnot(length(n) == 1L)
  
  ngr <- if (n < 0L) {
    max(nrow(x$data) + n, 0L)   
  } else { 
    min(n, nrow(x$data))
  }
  
  ngc <- if (n < 0L) {
    max(ncol(x$data) + n, 0L)   
  } else { 
    min(n, ncol(x$data))
  }
  
  list(data=x$data[seq_len(ngr), seq_len(ngc), drop = FALSE],
       AAC=head(x$AAC, n=n),
       AAF=head(x$AAF, n=n),
       included=list(subjects=head(x$included$subjects, n=n), 
                     snps=head(x$included$snps, n=n)),
       excluded=list(subjects=head(x$excluded$subjects, n=n), 
                     snps=head(x$excluded$snps, n=n))
  )
}

#' @export
tail.genotype <- function(x, n=6L, ...) {
  stopifnot(length(n) == 1L)
 
  nrx <- nrow(x$data)
  ngr <- if (n < 0L) {
    max(nrx + n, 0L)
  } else {
    min(n, nrx)
  }
  selr <- seq.int(to = nrx, length.out = ngr)
  
  ncx <- ncol(x$data)
  ngc <- if (n < 0L) {
    max(ncx + n, 0L)
  } else {
    min(n, ncx)
  }
  selc <- seq.int(to = ncx, length.out = ngc)
  

  list(data=x$data[selr, selc, drop = FALSE],
       AAC=tail(x$AAC, n=n),
       AAF=tail(x$AAF, n=n),
       included=list(subjects=tail(x$included$subjects, n=n), 
                     snps=tail(x$included$snps, n=n)),
       excluded=list(subjects=tail(x$excluded$subjects, n=n), 
                     snps=tail(x$excluded$snps, n=n))
  )
}