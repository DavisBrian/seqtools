library(dplyr)

context("Creating phenotype Object")

df <- data.frame(
  y   = c(3, 1, 4, 1, 5, 9, 3), 
  sex = c(0L, 0L, 1L, 0L, 1L, NA, 1L),
  bmi = c(0.3, 0.9, 0.5, 0.1, 0.4, 0.1, 0.3),
  sid = paste0("S", 1:7),
  gen = c("F", "F", "M", "F", "M", NA, "M"),
  stringsAsFactors=FALSE
)

.df <- as_data_frame(df)

res <- structure(
  .df, 
  formula = NULL, 
  idCol = "sid", 
  genderCol = NULL, 
  groupBy = NULL,
  included = .df$sid, 
  excluded = NULL,
  class = c("phenotype", class(.df))
)

# basic phenotype object creation
test_that("Phenotype object creation", {
  expect_equal(phenotype(df, .id="sid"), res)  
  

  
})



