#' @export
summary.genotype <- function(x) {
  if (is_genotype(x)) {
    gt <- x$data
  } else if (is.data.frame(x)) {
    gt <- as.matrix(x)
  } else if (is.matrix(x)) {
    gt <- x
  } else {
    stop("x must be of class 'genotype'")
  }
  
  gt_is_na <- is.na(gt)  
  
  # site statistics
  N_site <- colSums(!gt_is_na, na.rm=TRUE)
  RR_site <- colSums(gt == 0L, na.rm=TRUE)
  RA_site <- colSums(gt == 1L, na.rm=TRUE)
  AA_site <- colSums(gt == 2L, na.rm=TRUE)
  nMiss_site <- colSums(gt_is_na)
  MRate_site <- colMeans(gt_is_na)
  AAC_site <- colSums(gt, na.rm=TRUE)
  AAF_site <-colMeans(gt, na.rm=TRUE)/2.0
  
  site_metrics <- data.frame(N=N_site,
                             RR=RR_site,
                             RA=RA_site,
                             AA=AA_site, 
                             nMiss=nMiss_site,
                             MRate=MRate_site,
                             AAC=AAC_site, 
                             AAF=AAF_site, 
                             stringsAsFactors=FALSE
  )
  
  # sample statistics
  N_sample <- rowSums(!gt_is_na, na.rm=TRUE)  
  RR_sample <- rowSums(gt == 0L, na.rm=TRUE)
  RA_sample <- rowSums(gt == 1L, na.rm=TRUE)
  AA_sample <- rowSums(gt == 2L, na.rm=TRUE)
  nMiss_sample <- rowSums(gt_is_na)
  MRate_sample <- rowMeans(gt_is_na)
  #   #   sidx<-which(colSums(gt, na.rm=TRUE) == 1L)
  #   sidx<-which(AAC_site == 1L)
  #   nSingleton_sample <- rowSums(gt[sidx, , drop=FALSE], na.rm=TRUE)
  #   #   didx<-which(colSums(gt, na.rm=TRUE) == 2L)
  #   didx<-which(AAC_site == 2L)
  #   nDoubleton_sample <- rowSums(gt[didx, , drop=FALSE], na.rm=TRUE)
  
  sample_metrics <- data.frame(N=N_sample, 
                               RR=RR_sample, 
                               RA=RA_sample, 
                               AA=AA_sample, 
                               nMiss=nMiss_sample, 
                               MRate=MRate_sample, 
                               stringsAsFactors=FALSE
  )

  structure(
    list(
      site=site_metrics, 
      sample=sample_metrics
    ), 
    class = "summary_genotype"
  ) 
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
