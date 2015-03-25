#' seqtools: A collection of tools for sequence analysis.
#'
#' The seqtools package is a collection of tools to facilitate sequence
#' analysis.  It provides a common interface for working with phenotype data,
#' genotype data, and snp information.  Also, wrappers for common sequence
#' analysis test such as seqMeta, firth, linear regression, logistic regression,
#' survival analysis are provided.  
#' 
#' @section Firth functions:
#' \itemize{
#'  \item \code{burdenFirth}: runs the burden Firth test
#'  
#'  \item \code{codeToMinor}: ensures the genotype matrix is coded to the minor allele
#'  
#'  \item \code{imputeToMean}: imputes missing genotype values to the mean dosage
#'  
#'  \item \code{singsnpFirth}: runs the single snp Firth test
#'  
#' }
#'
#' @docType package
#' @name seqtools
#' @import dplyr
# @import logistf
NULL