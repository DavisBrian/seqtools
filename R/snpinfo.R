#' @export
snpinfo <- function(si, .snpNames="Name", .aggregateBy="gene", .chr=NULL, .pos=NULL, .ref=NULL, .alt=NULL, .filterBy=NULL, .filterFun=NULL, .otherCols=NULL) {

  cn <- c(.snpNames, .aggregateBy, .chr, .pos, .ref, .alt, .filterBy, .otherCols)
  
  # basic sanity check that all the column names we think we passed are actually in the si
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
     if (is.function(match.fun(.filterFun))) {
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
  
  
  structure(
    list(data=data.frame(si[, cn], .keep=keep),
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


