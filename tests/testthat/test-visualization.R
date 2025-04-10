library(testthat)
library(maternalhealthtools)
library(dplyr)
library(ggplot2)
library(randomForest)
library(tibble)
library(caret)
library(vip)
library(nnet)

if (!dir.exists("outputs/images")) {
  dir.create("outputs/images", recursive = TRUE)
}

test_that("plot_conf_matrix() creates a confusion matrix plot and saves a file", {
  conf_data <- data.frame(
    True = c("low risk", "mid risk", "high risk",
             "low risk", "mid risk", "high risk",
             "low risk", "mid risk", "high risk"),
    Predicted = c("low risk", "low risk", "low risk",
                  "mid risk", "mid risk", "mid risk",
                  "high risk", "high risk", "high risk"),
    Frequency = c(9, 17, 9, 10, 6, 9, 8, 14, 18),
    Percentage = c(33.3, 45.9, 25.0, 37.0, 16.2, 25.0, 29.6, 37.8, 50.0)
  )
  output_path <- file.path("outputs/images", "mlr_conf_matrix.png")
  plot <- plot_conf_matrix(conf_data, "mlr", "outputs/images")

  expect_s3_class(plot, "ggplot")
  expect_true(file.exists(output_path))
  expect_true("GeomTile" %in% class(plot$layers[[1]]$geom))
  expect_equal(plot$labels$x, "True Label")
  expect_equal(plot$labels$y, "Predicted Label")
})

test_that("plot_feature_importance() creates a plot and saves a file", {
  n <- 200
  helper_data <- tibble(
    Age = sample(18:50, n, TRUE),
    SystolicBP = sample(90:180, n, TRUE),
    DiastolicBP = sample(60:120, n, TRUE),
    BS = runif(n, 6, 15),
    BodyTemp = rnorm(n, 98.6, 0.7),
    HeartRate = sample(60:110, n, TRUE),
    RiskLevel = sample(c("low risk", "mid risk", "high risk"), n, TRUE)
  ) %>%
    mutate(RiskLevel = factor(RiskLevel))

  model <- randomForest(RiskLevel ~ ., data = helper_data, ntree = 100, importance = TRUE)
  plot <- plot_feature_importance(model, "outputs/images")

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$x, "Feature")
  expect_equal(plot$labels$y, "Importance")
  expect_true(file.exists("outputs/images/rf_feature_importance.png"))
})

test_that("plot_pred_prob() creates a smooth probability plot", {
  df <- tibble(
    BS = rep(seq(5.5, 8.0, by = 0.1), 3),
    Probability = runif(78, 0.2, 0.9),
    RiskLevel = rep(c("low risk", "mid risk", "high risk"), each = 26)
  )

  plot <- plot_pred_prob(df, "outputs/images")

  expect_s3_class(plot, "ggplot")
  expect_true("GeomSmooth" %in% class(plot$layers[[1]]$geom))
  expect_equal(plot$labels$x, "Blood Sugar (BS)")
  expect_equal(plot$labels$y, "Predicted Probability")
  expect_true(file.exists("outputs/images/blood_sugar_plot.png"))
})

test_that("plot functions throw errors for invalid input", {
  expect_error(plot_conf_matrix("not a df", "mlr", "outputs/images"))
  expect_error(plot_conf_matrix(data.frame(), "invalid_type", "outputs/images"))
  expect_error(plot_feature_importance("not a model", "outputs/images"))
  expect_error(plot_feature_importance(data.frame(), data.frame()))
  expect_error(plot_pred_prob("not a df", "outputs/images"))
})
