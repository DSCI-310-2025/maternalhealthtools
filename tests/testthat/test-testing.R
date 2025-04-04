library(testthat)
library(maternalhealthtools)
library(tibble)
library(nnet)
library(tidyverse)
library(randomForest)

test_that("testing() works with correct input", {
    # Sample training data matching real feature names and RiskLevel target
    train_data <- tibble(
        Age = c(25, 30, 40, 45),
        SystolicBP = c(120, 130, 140, 150),
        DiastolicBP = c(80, 85, 90, 95),
        BS = c(6.5, 7.0, 6.8, 7.1),
        BodyTemp = c(98.6, 99.0, 98.7, 98.5),
        HeartRate = c(75, 80, 85, 90),
        RiskLevel = as.factor(c("low risk", "mid risk", "high risk", "mid risk"))
    )

    # Fit
    model1 <- multinom(RiskLevel ~ ., data = train_data)
    model_path1 <- tempfile(fileext = ".rds")

    model2 <- randomForest(RiskLevel ~ ., data = train_data, ntree = 500, importance = TRUE)
    model_path2 <- tempfile(fileext = ".rds")

    # Save trained models to a temporary file
    saveRDS(model1, model_path1)
    saveRDS(model2, model_path2)

    # Create valid test data using the same feature names (no RiskLevel column)
    test_data <- tibble(
        Age = c(30, 40),
        SystolicBP = c(130, 140),
        DiastolicBP = c(85, 90),
        BS = c(6.9, 7.2),
        BodyTemp = c(98.9, 99.1),
        HeartRate = c(80, 85),
        RiskLevel = as.factor(c("low risk", "mid risk"))
    )

    # Test return of class predictions
    preds1 <- testing(model_path1, test_data)
    preds2 <- testing(model_path2, test_data)

    # Making sure that the returned value is a vector of "integers", since we have passed the parameter type="class"
    expect_type(preds1, "integer")
    expect_type(preds2, "integer")

    # Check to see if only 2 predictions are made
    expect_length(preds1, 2)
    expect_length(preds2, 2)

    # Test return with probabilities
    results1 <- testing(model_path1, test_data, return_probs = TRUE)
    results2 <- testing(model_path2, test_data)

    # Check to see if results variable stores 2 elements, one named predictions and another named probabilities
    expect_named(results1, c("predictions", "probabilities"))

    # Make sure that there are exactly 2 predictions made
    print(results2)
    expect_length(results1$predictions, 2)
    expect_length(results2, 2)

    # Make sure that there are exactly 2 rows in the probability matrix
    expect_equal(nrow(results1$probabilities), 2)

    # Delete temporary file
    unlink(model_path1)
    unlink(model_path2)
})

# This test makes sure that the path to the model's rds file exists
test_that("testing function cannot find the model file with given model path", {
    # This creates a path to a rds file that does not exist
    fake_model_path <- tempfile(fileext = ".rds")

    test_data <- tibble(
        Age = c(25),
        SystolicBP = c(120),
        DiastolicBP = c(80),
        BS = c(6.5),
        BodyTemp = c(98.6),
        HeartRate = c(75))

    expect_error(suppressWarnings(testing(fake_model_path, test_data)))
})

# This test makes sure that the test_data file is a tibble or DataFrame with 7 features/columns
test_that("testing function fails with invalid test_data input", {
    train_data <- tibble(
        Age = c(25, 30),
        SystolicBP = c(120, 130),
        DiastolicBP = c(80, 85),
        BS = c(6.5, 7.0),
        BodyTemp = c(98.6, 99.0),
        HeartRate = c(75, 80),
        RiskLevel = as.factor(c("low risk", "mid risk")))

    test_data <- tibble(
        Age = c(30, 40),
        SystolicBP = c(130, 140),
        DiastolicBP = c(85, 90),
        BS = c(6.9, 7.2),
        BodyTemp = c(98.9, 99.1),
        HeartRate = c(80, 85))

    model1 <- multinom(RiskLevel ~ ., data = train_data)
    model2 <- randomForest(RiskLevel ~ ., data = train_data, ntree = 500, importance = TRUE)
    model_path1 <- tempfile(fileext = ".rds")
    model_path2 <- tempfile(fileext = ".rds")
    saveRDS(model1, model_path1)
    saveRDS(model2, model_path2)

    # test_data must be a data frame
    expect_error(suppressWarnings(testing(model_path1, c(1, 2, 3))))
    expect_error(suppressWarnings(testing(model_path2, c(1, 2, 3))))

    # test_data must be a data frame with 7 features, missing RiskLevel feature
    expect_error(suppressWarnings(testing(model_path1, test_data)))
    expect_error(suppressWarnings(testing(model_path2, test_data)))

    bad_data <- tibble(
        Age = c(30),
        SystolicBP = c(130),
        BS = c(7.0),
        BodyTemp = c(99.0),
        HeartRate = c(80)
    )

    # Missing features DiastolicBP and RiskLevel
    expect_error(suppressWarnings(testing(model_path1, bad_data)))
    expect_error(suppressWarnings(testing(model_path2, bad_data)))

    unlink(model_path1)
    unlink(model_path2)
})

# This test tries setting return_probs to other values other than TRUE or FALSE
test_that("testing function fails with invalid return_probs type", {
    train_data <- tibble(
        Age = c(25, 30),
        SystolicBP = c(120, 130),
        DiastolicBP = c(80, 85),
        BS = c(6.5, 7.0),
        BodyTemp = c(98.6, 99.0),
        HeartRate = c(75, 80),
        RiskLevel = as.factor(c("low risk", "mid risk"))
    )

    test_data <- tibble(
        Age = c(30, 40),
        SystolicBP = c(130, 140),
        DiastolicBP = c(85, 90),
        BS = c(6.9, 7.2),
        BodyTemp = c(98.9, 99.1),
        HeartRate = c(80, 85),
        RiskLevel = as.factor(c("low risk", "mid risk"))
    )

    model1 <- multinom(RiskLevel ~ ., data = train_data)
    model2 <- randomForest(RiskLevel ~ ., data = train_data, ntree = 500, importance = TRUE)
    model_path1 <- tempfile(fileext = ".rds")
    model_path2 <- tempfile(fileext = ".rds")
    saveRDS(model1, model_path1)
    saveRDS(model2, model_path2)

    expect_error(testing(model_path1, test_data, return_probs = "yes"))
    expect_error(testing(model_path1, test_data, return_probs = "no"))
    expect_error(testing(model_path1, test_data, return_probs = "TRUE"))
    expect_error(testing(model_path1, test_data, return_probs = "FALSE"))
    expect_error(testing(model_path1, test_data, return_probs = 1))

    expect_error(testing(model_path2, test_data, return_probs = "yes"))
    expect_error(testing(model_path2, test_data, return_probs = "no"))
    expect_error(testing(model_path2, test_data, return_probs = "TRUE"))
    expect_error(testing(model_path2, test_data, return_probs = "FALSE"))
    expect_error(testing(model_path2, test_data, return_probs = 1))

    unlink(model_path1)
    unlink(model_path2)
})
