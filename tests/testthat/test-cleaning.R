library(testthat)
library(maternalhealthtools)
library(dplyr)
library(tibble)
library(tidyr)


test_that("check_na returns NA summary", {
  df <- data.frame(
    A = c(1, NA, 3),
    B = c(NA, NA, 3)
  )
  result <- check_na(df)

  expect_s3_class(result, "tbl_df")
  expect_equal(unname(result$na[result$feature == "A"]), 1)
  expect_equal(unname(result$na[result$feature == "B"]), 2)
})

test_that("get_targets returns unique target classes", {
  df <- data.frame(
    RiskLevel = c("low", "mid", "low", "high"),
    other_col = c(5, 10, 15, 20)
  )
  result <- get_targets(df, RiskLevel)

  expect_s3_class(result, "data.frame")
  expect_equal(sort(result$RiskLevel), c("high", "low", "mid"))
})

test_that("clean drops NAs and converts character target to factor", {
  df <- data.frame(
    Age = c(25, NA, 35),
    RiskLevel = c("low", "mid", "high")
  )

  cleaned <- clean(df, RiskLevel)

  expect_equal(nrow(cleaned), 2)
  expect_s3_class(cleaned$RiskLevel, "factor")
})

test_that("clean does not convert non-categorical targets", {
  df <- data.frame(
    Age = c(25, 30, 35),
    RiskScore = c(1, 2, 3)
  )

  cleaned <- clean(df, RiskScore)

  expect_equal(nrow(cleaned), 3)
  expect_type(cleaned$RiskScore, "double")
})
