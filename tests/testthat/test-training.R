library(testthat)
library(maternalhealthtools)
library(nnet)
library(randomForest)


test_that("train_mlr_model returns a multinomial regression model object", {
  df <- data.frame(
    Age = c(25, 30, 35),
    SystolicBP = c(120, 130, 110),
    DiastolicBP = c(80, 85, 75),
    BS = c(5.6, 6.2, 4.9),
    BodyTemp = c(98.6, 99.1, 97.9),
    HeartRate = c(72, 80, 76),
    RiskLevel = factor(c("low risk", "mid risk", "high risk"))
  )

  model <- training_mlr_model(df)
  expect_s3_class(model, "nnet")
})

test_that("training_mlr_model errors with missing RiskLevel column", {
  df_bad <- data.frame(
    Age = c(25, 30, 35),
    SystolicBP = c(120, 130, 110),
    DiastolicBP = c(80, 85, 75),
    BS = c(5.6, 6.2, 4.9),
    BodyTemp = c(98.6, 99.1, 97.9),
    HeartRate = c(72, 80, 76)
  )

  expect_error(training_mlr_model(df_bad), "object 'RiskLevel' not found")
})



test_that("train_rf_model returns a random forest model object", {
  df <- data.frame(
    Age = c(25, 30, 35),
    SystolicBP = c(120, 130, 110),
    DiastolicBP = c(80, 85, 75),
    BS = c(5.6, 6.2, 4.9),
    BodyTemp = c(98.6, 99.1, 97.9),
    HeartRate = c(72, 80, 76),
    RiskLevel = factor(c("low risk", "mid risk", "high risk"))
  )

  model <- training_rf_model(df)
  expect_s3_class(model, "randomForest")
})

test_that("training_rf_model errors with missing RiskLevel column", {
  df_bad <- data.frame(
    Age = c(25, 30, 35),
    SystolicBP = c(120, 130, 110),
    DiastolicBP = c(80, 85, 75),
    BS = c(5.6, 6.2, 4.9),
    BodyTemp = c(98.6, 99.1, 97.9),
    HeartRate = c(72, 80, 76)
  )

  expect_error(training_rf_model(df_bad), "object 'RiskLevel' not found")
})

test_that("training_rf_model errors with empty data", {
  expect_error(training_rf_model(data.frame()), "'.' in formula and no 'data' argument")
})
