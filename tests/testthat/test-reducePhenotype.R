context("Reduce phenotype object")

df <- data.frame(
  y   = c(3, 1, 4, 1, 5, 9, 3), 
  sex = c(0L, 0L, 1L, 0L, 1L, NA, 1L),
  bmi = c(0.3, 0.9, 0.5, 0.1, 0.4, 0.1, 0.3),
  sid = paste0("S", 1:7),
  gen = c("F", "F", "M", "F", "M", NA, "M"),
  stringsAsFactors=FALSE
)


# formula
test_that("Formula", {
  
  p <- phenotype(df)
  res <- p
  res$data <- na.omit(p$data)
  expect_equal(reducePhenotype(p), res)  

  p <- phenotype(df, formula=y~.) 
  res <- p
  res$data <- na.omit(p$data)
  expect_equal(reducePhenotype(p), res)  

  p <- phenotype(df, formula=y~sex+bmi)
  res <- p
  res$data <- na.omit(get_all_vars(formula=y~sex+bmi, p$data))
  expect_equal(reducePhenotype(p), res)  
  
  p <- phenotype(df, formula=y~sex+log(bmi))
  res <- p
  res$data <- na.omit(get_all_vars(formula=y~sex+bmi, p$data))
  expect_equal(reducePhenotype(p), res)  
  
})


# need to add a few more test 