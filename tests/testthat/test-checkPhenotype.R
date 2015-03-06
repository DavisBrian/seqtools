context("Check Phenotype")

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
  expect_null(checkPhenotype(df))
  expect_null(checkPhenotype(df, formula=y~.))
  expect_null(checkPhenotype(df, formula=y~1))
  expect_null(checkPhenotype(df, formula=y~sex+bmi))
  expect_null(checkPhenotype(df, formula=y~sex+log(bmi)))
  
  expect_error(checkPhenotype(df, formula=y~sex+bmi2))
              
})

# id
test_that("Subject ID", {

  expect_null(checkPhenotype(df, id="sid"))
  expect_null(checkPhenotype(df[1:5, ], id="sid"))
  expect_null(checkPhenotype(df, id="sid"))
  
  expect_error(checkPhenotype(df, id="Subs"))
  expect_error(checkPhenotype(df, id="gen"))
  
})

# gender
test_that("Gender", {

  # Integer
  df_I <- data.frame(
    y   = c(3, 1, 4, 1, 5, 9, 3), 
    sex = c(0L, 0L, 1L, 0L, 1L, NA, 1L),
    sexM = rep(1L, 7), 
    sexF = rep(0L, 7),
    sexMna = c(NA, rep(1L, 6)),
    sexFna = c(NA, rep(1L, 6)),
    sex3 = c(0L, 0L, 1L, 0L, 1L, 2L, 1L), 
    sex12 = c(1L, 2L, 2L, 1L, 2L, 1L, 2L), 
    stringsAsFactors=FALSE
  )
  
  expect_null(checkPhenotype(df_I, gender="sex"))
  expect_null(checkPhenotype(df_I, gender="sexM"))
  expect_null(checkPhenotype(df_I, gender="sexF"))
  expect_null(checkPhenotype(df_I, gender="sexMna"))
  expect_null(checkPhenotype(df_I, gender="sexFna"))
  expect_error(checkPhenotype(df_I, gender="sex3"))
  expect_error(checkPhenotype(df_I, gender="sex12"))
  
  # Character
  df_C <- data.frame(
    y   = c(3, 1, 4, 1, 5, 9, 3), 
    gen = c("F", "F", "M", "F", "M", NA, "M"),
    genM = rep("M", 7), 
    genF = rep("F", 7),
    genMna = c(NA, rep("M", 6)),
    genFna = c(NA, rep("F", 6)),
    gen3 = c("F", "F", "M", "F", "M", "m", "M"),
    gen12 = c("FEMALE", "FEMALE", "MALE", "FEMALE", "MALE", "MALE", "MALE"),
    stringsAsFactors=FALSE
  )
  
  expect_null(checkPhenotype(df_C, gender="gen"))
  expect_null(checkPhenotype(df_C, gender="genM"))
  expect_null(checkPhenotype(df_C, gender="genF"))
  expect_null(checkPhenotype(df_C, gender="genMna"))
  expect_null(checkPhenotype(df_C, gender="genFna"))
  
  expect_error(checkPhenotype(df_C, gender="gen3"))
  expect_error(checkPhenotype(df_C, gender="gen12"))
  
  # Logical

  # Factor
  
  # Numeric
  
})


# include
test_that("Include", {
  expect_error(checkPhenotype(df, id=NULL, include=paste0("S", 1:7)))
  expect_null(checkPhenotype(df, id="sid", include=paste0("S", 1:7)))
  
  expect_null(checkPhenotype(df, id="sid", include=c("S1", "S3", "D2")))
  expect_error(checkPhenotype(df, id="sid", include=paste0("D", 1:7)))
})


# exclude
test_that("Exclude", {
  expect_null(checkPhenotype(df, id="sid", exclude=c("S1", "S3", "D2")))
  expect_warning(checkPhenotype(df, id=NULL, exclude=paste0("S", 1:7)))
  expect_warning(checkPhenotype(df, id="sid", exclude=paste0("D", 1:7)))
  expect_error(checkPhenotype(df, id="sid", exclude=paste0("S", 1:7)))
})


# full
test_that("Include/Exclude", {
  expect_null(checkPhenotype(df, id="sid", include=c("S1", "S2", "S3"), exclude=c("S5", "S6", "S7")))
  expect_null(checkPhenotype(df, id="sid", include=c("S1", "S2", "S3", "D1"), exclude=c("S5", "S6", "S7")))  
  expect_null(checkPhenotype(df, id="sid", include=c("S1", "S2", "S3"), exclude=c("S5", "S6", "S7", "D2"))) 
  expect_error(checkPhenotype(df, id="sid", include=c("S1", "S2", "S3"), exclude=c("S3", "S4", "S5")))
  expect_error(checkPhenotype(df, id="sid", include=c("S1", "S2", "S3", "D1"), exclude=c("S3", "S4", "S5")))              
  expect_error(checkPhenotype(df, id="sid", include=c("S1", "S2", "S3"), exclude=c("S3", "S4", "S5", "D2")))                
})

