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

# testing cases

test_that("`visualization()` should return the correct file name in the specified location.", {
  helper_conf_table <- data.frame(
                True = c("low risk", "mid risk", "high risk",
                        "low risk", "mid risk", "high risk",
                        "low risk", "mid risk", "high risk"),
                Predicted = c("low risk", "low risk", "low risk",
                                "mid risk", "mid risk", "mid risk",
                                "high risk", "high risk", "high risk"),
                Frequency = c(9, 17, 9, 10, 6, 9, 8, 14, 18),
                Percentage = c(33.3, 45.9, 25.0, 37.0, 16.2, 25.0, 29.6, 37.8, 50.0)
                )
  visualization("conf_matrix", helper_conf_table, "mlr", "outputs/images")
  expect_true(file.exists("outputs/images/mlr_conf_matrix.png"))
})


test_that("`visualization()` should use `geom_tile()` for `conf_matrix` and check if using the right data and if axes have the right labels,
          use `geom_smooth` for `pred_prob` and check if using the right data and if axes have the right labels,
          map x-axis to Feature and y-axis to Importance for 'feature_importance'", {

            set.seed(123)
            # create conf_matrix using visualization()
            helper_conf_table <- data.frame(
                True = c("low risk", "mid risk", "high risk",
                        "low risk", "mid risk", "high risk",
                        "low risk", "mid risk", "high risk"),
                Predicted = c("low risk", "low risk", "low risk",
                                "mid risk", "mid risk", "mid risk",
                                "high risk", "high risk", "high risk"),
                Frequency = c(9, 17, 9, 10, 6, 9, 8, 14, 18),
                Percentage = c(33.3, 45.9, 25.0, 37.0, 16.2, 25.0, 29.6, 37.8, 50.0)
                )
            helper_plot_conf <- visualization("conf_matrix", helper_conf_table, "mlr", "outputs/images")

            # create pred_prob plot using visualization()
            risk_levels <- rep(c("low risk", "mid risk", "high risk"), length.out = 300)
            risk_levels <- sample(risk_levels)
            prob_long <- data.frame(
                BS = round(sample(c(7.19, 8.06, 8.43, 11.22, 11.61), 300, replace = TRUE) + rnorm(300, mean = 0, sd = 0.05), 2),
                RiskLevel = risk_levels,
                Probability = round(runif(300, min = 0.1, max = 0.7), 4)
            )
            helper_plot_pred_prob <- visualization("pred_prob", prob_long, NULL, "outputs/images")

            # create feature_importances plot using visualization()
            n <- 300
            Age <- sample(15:60, n, replace = TRUE)
            SystolicBP <- sample(85:160, n, replace = TRUE)
            DiastolicBP <- sample(60:100, n, replace = TRUE)
            BS <- round(runif(n, min = 5.5, max = 20), 2)
            BodyTemp <- round(rnorm(n, mean = 98.5, sd = 1), 1)
            HeartRate <- sample(60:100, n, replace = TRUE)

            RiskLevel <- ifelse(
              SystolicBP > 140 | BS > 13 | HeartRate > 85,
              "high risk",
              ifelse(
                SystolicBP < 100 & BS < 8 & HeartRate < 75,
                "low risk",
                "mid risk"))
            helper_data <- data.frame(
              Age,
              SystolicBP,
              DiastolicBP,
              BS,
              BodyTemp,
              HeartRate,
              RiskLevel) %>%
              mutate(RiskLevel = as.factor(RiskLevel))

            helper_rf_model <- randomForest(RiskLevel ~ ., data = helper_data, ntree = 500, importance = TRUE)
            helper_plot_feat_imp <- visualization("feature_importance", helper_rf_model, NULL, "outputs/images")

            # test cases for confidence matrix
            expect_true("GeomTile" %in% c(class(helper_plot_conf$layers[[1]]$geom)))
            expect_true(helper_plot_conf$labels$x == "True Label")
            expect_true(helper_plot_conf$labels$y == "Predicted Label")
            expect_true(all(helper_plot_conf$data[1:4] == helper_conf_table))

            # test cases for predicted probability
            expect_true("GeomSmooth" %in% c(class(helper_plot_pred_prob$layers[[1]]$geom)))
            expect_true(helper_plot_pred_prob$labels$x == "Blood Sugar (BS)")
            expect_true(helper_plot_pred_prob$labels$y == "Predicted Probability")
            expect_true(all(helper_plot_pred_prob$data ==prob_long))

            # test cases for feature importances
            expect_true(helper_plot_feat_imp$labels$x == "Feature")
            expect_true(helper_plot_feat_imp$labels$y == "Importance")
})


test_that("`visualization` should throw an error when invalid parameters are used.", {
  expect_error(visualization("pred_prob", prob_long, NULL, 123))
  expect_error(visualization("pred_prob", prob_long, "outputs/images"))
  expect_error(visualization(pred_prob, prob_long, NULL, "outputs/images"))
  expect_error(visualization("pred_prob", "prob_long", NULL, "outputs/images"))
  expect_error(visualization("pred_prob", prob_long, "mlr", "outputs/images"))
  expect_error(visualization("predicted_prob", prob_long, NULL, "outputs/images"))
  expect_error(visualization("conf_matrix", mlr_table, mlr, "outputs/images"))
  expect_error(visualization("conf_matrix", mlr_table, "regression", "outputs/images"))
  expect_error(visualization("confusion_matrix", mlr_table, "mlr", "outputs/images"))
})

