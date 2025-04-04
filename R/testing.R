# R/testing.R

#' Generate predictions from a trained classification model
#'
#' This function loads a trained classification model from an `.rds` file and uses it to
#' make predictions on a test dataset. It can also return class probabilities but it is disabled by default.
#'
#' @param model_file Character. Path to the saved `.rds` model file
#' @param test_data Data frame or tibble. Test dataset with both regular and target feature
#' @param return_probs Logical. If TRUE, the function also returns class probabilities
#'
#' @return
#' If `return_probs = FALSE`, returns a character vector of predicted class labels.
#' If `return_probs = TRUE`, returns a named list with:
#'  - predictions: A character vector of predicted class labels
#'  - probabilities: A matrix or data frame of class probabilities
#'
#' @export
#' @examples
#' \dontrun{
#' testing("model.rds", test_data)
#' testing("model.rds", test_data, return_probs = TRUE)
#' }


testing <- function(model_file, test_data, return_probs = FALSE) {
    if (!is.logical(return_probs) || length(return_probs) != 1) {
        stop("return_probs must be TRUE or FALSE")
        }

    model <- readRDS(model_file)
    test_features <- test_data %>% dplyr::select(-RiskLevel)
    predictions <- stats::predict(model, newdata = test_features, type = "class")

    if (return_probs) {
        probs <- stats::predict(model, newdata = test_features, type = "probs")
        return(list(predictions = predictions, probabilities = probs))
    }

    return(predictions)
}
