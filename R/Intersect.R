#' @export
Intersect <- function(geno=NULL, pheno=NULL, si=NULL, ...) {  
  
  # find the subject intersection
  subjects <- if(!is.null(geno) && !is.null(pheno)) {
    if(!is_genotype(geno)) {
      stop("geno must be a 'genotype' object")
    }
    if(!is_phenotype(pheno)) {
      stop("pheno must be a 'phenotype' object")
    }
    intersect(rownames(geno$data), rownames(pheno$data))
  } else {
    NULL
  }
  
  # find the snps intersection
  snps <- if(!is.null(geno) && !is.null(si)) {
    if(!is_genotype(geno)) {
      stop("geno must be a 'genotype' object")
    }
    if(!is_snpinfo(si)) {
      stop("si must be a 'snpinfo' object")
    }  
    intersect(colnames(geno$data), si$data[ , si$snpNames])
  } else {
    NULL
  }
  
  if (length(subjects) == 0L && length(snps) == 0L) {
    stop("No subjects or snps in common")
  } else if (length(subjects) > 0L && length(snps) > 0L) {
    list(subjects=subjects,
                snps=snps) 
  } else if (length(subjects) > 0L) {
    subjects
  } else if (length(snps) > 0L) {
    snps
  } else {
    stop("Unable to determine subjects and snps")
  }
}

