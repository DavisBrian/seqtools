context("Basic Checks")

df <- data.frame(
  y   = c(3, 1, 4, 1, 5, 9, 3), 
  sex = c(0L, 0L, 1L, 0L, 1L, NA, 1L),
  bmi = c(0.3, 0.9, 0.5, 0.1, 0.4, 0.1, 0.3),
  sid = paste0("S", 1:7),
  gen = as.factor(c("F", "F", "M", "F", "M", NA, "M")),
  kep = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors=FALSE
)



# formula
test_that("check_colnames", {
  expect_null(check_colnames(df, "sex"))
  expect_null(check_colnames(df, c("sex", "sid"))) 
  expect_null(check_colnames(df, all.vars(formula(y~sex+log(bmi)))))
  expect_error(check_colnames(df, character(0)))
  expect_error(check_colnames(df[ , FALSE, drop=FALSE], "sex"))
  expect_error(check_colnames(df, 1:3))
  expect_error(check_colnames(df, "sx"))
  expect_error(check_colnames(df, NA))
  expect_error(check_colnames(df, NULL))
  expect_error(check_colnames(df, c("y", NA, "sid")))   
  expect_error(check_colnames(df, c("y", "sx", "gen"))) 
})

test_that("check_type", {
  # integer tests
  expect_null(check_type(df, "sex", "integer")) 
  expect_error(check_type(df, "sex", "int")) 
  expect_null(check_type(df, "sex", "numeric")) 
  expect_error(check_type(df, "sex", "num")) 
  expect_error(check_type(df, "sex", "character"))
  # double tests
  expect_null(check_type(df, "y", "numeric")) 
  expect_null(check_type(df, "y", "double")) 
  expect_error(check_type(df, "y", "integer"))   
  expect_error(check_type(df, "y", "factor"))  
  # character test
  expect_null(check_type(df, "sid", "character")) 
  expect_error(check_type(df, "sid", "factor")) 
  expect_error(check_type(df, "sid", "integer")) 
  # factor tests
  expect_null(check_type(df, "gen", "factor")) 
  expect_error(check_type(df, "gen", "character")) 
  expect_error(check_type(df, "gen", "integer")) 
  expect_error(check_type(df, "gen", "numeric")) 
  expect_error(check_type(df, "gen", "test")) 
  # logical test
  expect_null(check_type(df, "kep", "logical")) 
  expect_error(check_type(df, "kep", "log")) 
  expect_error(check_type(df, "kep", "integer")) 
  expect_error(check_type(df, "kep", "numeric")) 
  expect_error(check_type(df, "kep", "character"))   
  expect_error(check_type(df, "kep", "factor")) 
  # other tests
  expect_error(check_type(df, c("y", "bmi") , "numeric")) 
  expect_error(check_type(df, "y" , c("numeric", "double"))) 
})