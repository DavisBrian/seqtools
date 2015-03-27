# TBD / Ideas
# - [ method to keep class and AAC / AFF correct  when subsetting
# - c method to combine genotype matrices

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
#  - model type (additive, etc) support
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
  new_class <- class(data)
  
  structure(
    data,
    AAC=colSums(data, na.rm=TRUE),
    AAF=colMeans(data, na.rm=TRUE)/2.0,
    included=list(subjects=rownames(data), snps=colnames(data)),
    excluded=list(subjects=setdiff(rownames(Z), rownames(data)), 
                  snps=setdiff(colnames(Z), colnames(data))),
    class = c("genotype", new_class)
  )
  
  #   structure(
  #     list(data=data,
  #          AAC=colSums(data, na.rm=TRUE),
  #          AAF=colMeans(data, na.rm=TRUE)/2.0,
  #          included=list(subjects=rownames(data), snps=colnames(data)),
  #          excluded=list(subjects=setdiff(rownames(Z), rownames(data)), 
  #                        snps=setdiff(colnames(Z), colnames(data)))
  #     ),
  #     class = "genotype"
  #   )    
}

#' @rdname genotype
#' @export
is.genotype <- function(x) inherits(x, "genotype")

# [TBD] BASIC PRINT METHOD that way we don't get pages of crap

#' @export
head.genotype <- function(x, n=6L, ...) {    
  stopifnot(length(n) == 1L)
  
  ngr <- if (n < 0L) {
    max(nrow(x) + n, 0L)   
  } else { 
    min(n, nrow(x))
  }
  
  ngc <- if (n < 0L) {
    max(ncol(x) + n, 0L)   
  } else { 
    min(n, ncol(x))
  }
  
  x[seq_len(ngr), seq_len(ngc), drop = FALSE]
}

#' @export
tail.genotype <- function(x, n=6L, ...) {
  stopifnot(length(n) == 1L)
 
  nrx <- nrow(x)
  ngr <- if (n < 0L) {
    max(nrx + n, 0L)
  } else {
    min(n, nrx)
  }
  selr <- seq.int(to = nrx, length.out = ngr)
  
  ncx <- ncol(x)
  ngc <- if (n < 0L) {
    max(ncx + n, 0L)
  } else {
    min(n, ncx)
  }
  selc <- seq.int(to = ncx, length.out = ngc)
  

  x[selr, selc, drop = FALSE]
}

# metadata  functions  --------------------------------------------------------

# get functions
#get_subjects.genotype <- function(x) rownames(x)
#get_snps.genotype <- function(x) colnames(x)
#' @export
get_subjects.genotype <- function(x, excluded=FALSE) {
  stopifnot(length(excluded) == 1L)
  if (excluded) {
    attr(x, "excluded")[["subjects"]]  
  } else {
    attr(x, "included")[["subjects"]]
  }
}

#' @export
get_snps.genotype <- function(x, excluded=FALSE, ...) {
  stopifnot(length(excluded) == 1L)
  if (excluded) {
    attr(x, "excluded")[["snps"]]  
  } else {
    attr(x, "included")[["snps"]]
  }
}



#' @export
get_AAC.genotype <- function(x) attr(x, "AAC")

#' @export
get_AAF.genotype <- function(x) attr(x, "AAF")

# Summary functions  ----------------------------------------------------------

#' @export
summary.genotype <- function(x) {
  x <- if (is.genotype(x) || is.matrix(x)) {
    x
  } else if (is.data.frame(x)) {
    as.matrix(x)
  } else {
    stop("x must be of class 'genotype'")
  }
    
  structure(
    list(
      site=site_metrics_genotype(x), 
      sample=sample_metrics_genotype(x)
    ), 
    class = "summary_genotype"
  ) 
}

site_metrics_genotype <- function(gt) {
  gt_is_na <- is.na(gt)  
  
  N_site <- colSums(!gt_is_na, na.rm=TRUE)
  RR_site <- colSums(gt == 0L, na.rm=TRUE)
  RA_site <- colSums(gt == 1L, na.rm=TRUE)
  AA_site <- colSums(gt == 2L, na.rm=TRUE)
  nMiss_site <- colSums(gt_is_na)
  MRate_site <- colMeans(gt_is_na)
  AAC_site <- colSums(gt, na.rm=TRUE)
  AAF_site <-colMeans(gt, na.rm=TRUE)/2.0
  
  data_frame(id = colnames(gt),
             N=N_site,
             RR=RR_site,
             RA=RA_site,
             AA=AA_site, 
             Nmiss=nMiss_site,
             MissRate=MRate_site,
             AAC=AAC_site, 
             AAF=AAF_site 
  )
  
}

sample_metrics_genotype <- function(gt) {
  gt_is_na <- is.na(gt) 
  
  # sample statistics
  N_sample <- rowSums(!gt_is_na, na.rm=TRUE)  
  RR_sample <- rowSums(gt == 0L, na.rm=TRUE)
  RA_sample <- rowSums(gt == 1L, na.rm=TRUE)
  AA_sample <- rowSums(gt == 2L, na.rm=TRUE)
  nMiss_sample <- rowSums(gt_is_na)
  MRate_sample <- rowMeans(gt_is_na)

  data_frame(id = rownames(gt),
             N=N_sample, 
             RR=RR_sample, 
             RA=RA_sample, 
             AA=AA_sample, 
             NMiss=nMiss_sample, 
             MissRate=MRate_sample
  )  
}

#' @export
print.summary_genotype <- function(x, ...) {
  rule("Site Level Metrics")
  if (is.data.frame(x$site)) {
    print.data.frame(x$site, right=FALSE, row.names=FALSE)   
  } else {
    print("None") 
  }
  rule("Sample Level Metrics")
  if (is.data.frame(x$sample)) {
    print.data.frame(x$sample, right=FALSE, row.names=FALSE)   
  } else {
    print("None") 
  }
  
}

#' @export
head.summary_genotype <- function(x, n=6L, ...) {
  stopifnot(length(n) == 1L)
  
  list(site=head(x$site, n=n),
       sample=head(x$sample, n=n)
  )
}

#' @export
tail.summary_genotype <- function(x, n=6L, ...) {
  stopifnot(length(n) == 1L)
  
  list(site=tail(x$site, n=n),
       sample=tail(x$sample, n=n)
  )
}
