
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maternalhealthtools

<!-- badges: start -->
[![R-CMD-check](https://github.com/DSCI-310-2025/maternalhealthtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DSCI-310-2025/maternalhealthtools/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/DSCI-310-2025/maternalhealthtools/graph/badge.svg)](https://app.codecov.io/gh/DSCI-310-2025/maternalhealthtools)
[![test-coverage](https://github.com/DSCI-310-2025/maternalhealthtools/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/DSCI-310-2025/maternalhealthtools/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The goal of maternalhealthtools is to provide reusable functions for
cleaning, training, testing, and visualizing maternal health data. It
includes tools for handling missing or inconsistent data, training and
testing multinomial logistic regression and random forest models, and
generating plot for exploratory analysis and model interpretation.

## Installation

You can install the development version of maternalhealthtools from:

[GitHub](https://github.com/DSCI-310-2025/maternalhealthtools.git) with:

``` r
# install.packages("devtools")
devtools::install_github("DSCI-310-2025/maternalhealthtools")
```

## Usage

A common task when dealing with maternal health data involves checking
for missing values, cleaning data, and examining unique values from the
target column. This is where functions like check_na(), clean(), and
get_targets() are useful.

``` r
library(maternalhealthtools)

# Sample dataset
df <- data.frame(
  Age = c(25, NA, 35, 40, 45, 30),
  BloodPressure = c(120, 130, NA, 110, 140, 125),
  RiskLevel = c("low risk", "mid risk", "high risk", NA, "high risk", "mid risk"),
  stringsAsFactors = FALSE
)

check_na(df)
#> # A tibble: 3 × 2
#>   feature          na
#>   <chr>         <dbl>
#> 1 Age               1
#> 2 BloodPressure     1
#> 3 RiskLevel         1
```

check_na() summarizes the number of missing values for each column. This
makes it easier to spot where cleaning is needed.

``` r
cleaned_df <- clean(df, RiskLevel)
cleaned_df
#>   Age BloodPressure RiskLevel
#> 1  25           120  low risk
#> 2  45           140 high risk
#> 3  30           125  mid risk
```

clean() ensures that the target variable is properly formatted as a
factor and removes the missing data.

``` r
get_targets(cleaned_df, RiskLevel)
#>   RiskLevel
#> 1  low risk
#> 2 high risk
#> 3  mid risk
```

get_targets() returns the unique values of the target column. This is
especially useful when working with large datasets and for understanding
the class distribution before modeling.

``` r
# Example training data
train_data <- data.frame(
  Age = c(25, 30, 35),
  SystolicBP = c(120, 130, 110),
  DiastolicBP = c(80, 85, 75),
  BS = c(5.6, 6.2, 4.9),
  BodyTemp = c(98.6, 99.1, 97.9),
  HeartRate = c(72, 80, 76),
  RiskLevel = factor(c("low risk", "mid risk", "high risk"))
)

mlr_model <- training_mlr_model(train_data)
summary(mlr_model)
#> Call:
#> nnet::multinom(formula = RiskLevel ~ ., data = train_data, trace = FALSE)
#> 
#> Coefficients:
#>          (Intercept)       Age SystolicBP DiastolicBP        BS   BodyTemp
#> low risk  0.08007772 -31.23502   9.632075    6.417592 0.6714557   8.340112
#> mid risk -0.35495554 -10.62350  24.546617    5.174198 2.2761942 -31.170615
#>             HeartRate
#> low risk -20.81487193
#> mid risk  -0.09997695
#> 
#> Std. Errors:
#>          (Intercept)          Age  SystolicBP  DiastolicBP           BS
#> low risk 2.71479e+36 6.786981e+37 3.25775e+38 2.171833e+38 1.520283e+37
#> mid risk 2.71479e+36 6.786981e+37 3.25775e+38 2.171833e+38 1.520283e+37
#>              BodyTemp   HeartRate
#> low risk 2.676783e+38 1.95465e+38
#> mid risk 2.676783e+38 1.95465e+38
#> 
#> Residual Deviance: 0 
#> AIC: 12
```

training_mlr_model() trains a multinomial logistic regression model
based on the data.

``` r
rf_model <- training_rf_model(train_data)
print(rf_model)
#> 
#> Call:
#>  randomForest(formula = RiskLevel ~ ., data = train_data, ntree = 500,      importance = TRUE) 
#>                Type of random forest: classification
#>                      Number of trees: 500
#> No. of variables tried at each split: 2
#> 
#>         OOB estimate of  error rate: 100%
#> Confusion matrix:
#>           high risk low risk mid risk class.error
#> high risk         0        1        0           1
#> low risk          1        0        0           1
#> mid risk          0        1        0           1
```

Similarly, training_rf_model() builds a random forest model using the
default of 500 trees.

The testing() function loads a saved classification model (e.g., .rds
file) and applies it to a test dataset to generate predictions. The
class probabilities are also available but is disabled by default.

``` r
# Example usage
# testing("path/to/mlr_model.rds", test_data)
```

Note: Since the function depends on a trained model file, examples are
not run in the README file.

The visualization() helps generate three types of plots:

1.  Confusion Matrix plot
2.  Random Forest Feature Importance plot
3.  Predicted Probability vs Blood Sugar plot

This function is useful for model visualization and interpretation. It
saves the generated plot to a file in the specified output directory.

``` r
# Example usage
# 1. Confusion Matrix Plot example

# helper_conf_table <- data.frame(
#                True = c("low risk", "mid risk", "high risk",
#                        "low risk", "mid risk", "high risk",
#                        "low risk", "mid risk", "high risk"),
#                Predicted = c("low risk", "low risk", "low risk",
#                                "mid risk", "mid risk", "mid risk",
#                                "high risk", "high risk", "high risk"),
#                Frequency = c(9, 17, 9, 10, 6, 9, 8, 14, 18),
#                Percentage = c(33.3, 45.9, 25.0, 37.0, 16.2, 25.0, 29.6, 37.8, 50.0))
#                
# visualization("conf_matrix", helper_conf_table, conf_type = "mlr", output_dir = "outputs")


# 2. Random Forest Feature Importance plot

# rf_model <- training_rf_model(train_data)

# visualization("feature_importance", rf_model, conf_type = NULL, output_dir = "outputs")

# 3. Predicted Probability vs Blood Sugar plot

# prob_table <- data.frame(
#   BS = c(4.5, 5.0, 5.5, 6.0, 6.5, 7.0),
#   Probability = c(0.2, 0.3, 0.5, 0.6, 0.7, 0.9),
#   RiskLevel = factor(c("low risk", "low risk", "mid risk", "mid risk", "high risk", "high risk"))
#   )

# visualization("pred_prob", prob_table, conf_type = NULL, output_dir = "outputs")
```

Note: Since the function depends on input and output files, examples are
not run in the README file. For more information on the visualization()
function, reading the visualization.R script is recommended.

## Position in the R Package Ecosystem

The maternalhealthtools package provides a small set of helper functions
to support data analysis workflows. It combines common tasks such as
cleaning data, training and testing models, and generating
visualizations. While the functions in this package are built on widely
used R packages like dplyr, randomForest, and ggplot2, they are meant to
simplify repetitive steps. Compared to other packages,
maternalhealthtools provides a lighter and more specific use case like
maternal health classification. It offers a predefined workflow that
reduces the amount of repeated code.
