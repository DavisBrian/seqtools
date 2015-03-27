#' @export
get_snps <- function(x, ...) UseMethod("get_snps", x)

#' @export
get_snps.default <- function(x, ...) "Unknown class"

#' @export
get_subjects <- function(x, ...) UseMethod("get_subjects", x)

#' @export
get_subjects.default <- function(x, ...) "Unknown class"

# #' @export
# get_snps_snpinfo <- function(x) x[[attr(x, "snpNames")]]

# #' @export
# get_attributes <- function(x) UseMethod("get_attributes")
# 
# #' @export
# get_attributes.default <- function(x) "Unknown class"
# 
# #' @export
# get_attributes.phenotype <- function(x) {
#   list(
#     formula = attr(x, "formula"),
#     idCol = attr(x, "idCol"),
#     genderCol = attr(x, "genderCol"),
#     groupBy = attr(x, "groupBy"),
#     included = attr(x, "included"),
#     excluded = attr(x, "excluded"),
#     class = attr(x, "class")
#   )
# }
# 
# set_phenotype_attributes <- function(data, .attr) {
#   attr(data, "formula") <- .attr$formula
#   attr(data, "idCol") <- .attr$idCol
#   attr(data, "genderCol") <- .attr$genderCol
#   attr(data, "groupBy") <- .attr$groupBy
#   attr(data, "included") <- .attr$included
#   attr(data, "excluded") <- .attr$excluded
#   attr(data, "class")  <- .attr$class 
#   
#   data
# }
# 
