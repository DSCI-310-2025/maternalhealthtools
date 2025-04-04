# R/training.R

#' Train Multinomial Logistic Regression Model
#'
#' @param train_data A dataframe containing the training data
#' @return A trained multinomial model
#' @export
#'
#'
#' @examples
#' \dontrun{
#' training_mlr_model(train_data)
#' }
#'
#'
training_mlr_model <- function(train_data) {
  model <- nnet::multinom(RiskLevel ~ ., data = train_data, trace = FALSE)
  return(model)
}

#' Train Random Forest Model
#'
#' @param train_data A dataframe containing the training data
#' @return A trained random forest model
#' @export
#'
#'
#' @examples
#' \dontrun{
#' training_rf_model(train_data)
#' }
training_rf_model <- function(train_data) {
  model <- randomForest::randomForest(RiskLevel ~ ., data = train_data, ntree = 500, importance = TRUE)
  return(model)
}
