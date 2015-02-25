context("Creating Phenotype Object")

df <- data.frame(
  y   = c(3, 1, 4, 1, 5, 9, 3), 
  sex = c(0L, 0L, 1L, 0L, 1L, NA, 1L),
  bmi = c(0.3, 0.9, 0.5, 0.1, 0.4, 0.1, 0.3),
  sid = paste0("S", 1:7),
  gen = c("F", "F", "M", "F", "M", NA, "M"),
  stringsAsFactors=FALSE
)


res <- list(data=df, formula=NULL, id=NULL, gender=NULL, include=as.character(1:length(df$y)), exclude=NULL)
class(res) <- "phenotype"

# formula
test_that("Formula", {
  expect_equal(phenotype(df), res)  
  

  
})
