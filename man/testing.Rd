% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testing.R
\name{testing}
\alias{testing}
\title{Generate predictions from a trained classification model}
\usage{
testing(model_file, test_data, return_probs = FALSE)
}
\arguments{
\item{model_file}{Character. Path to the saved \code{.rds} model file}

\item{test_data}{Data frame or tibble. Test dataset with both regular and target feature}

\item{return_probs}{Logical. If TRUE, the function also returns class probabilities}
}
\value{
If \code{return_probs = FALSE}, returns a character vector of predicted class labels.
If \code{return_probs = TRUE}, returns a named list with:
\itemize{
\item predictions: A character vector of predicted class labels
\item probabilities: A matrix or data frame of class probabilities
}
}
\description{
This function loads a trained classification model from an \code{.rds} file and uses it to
make predictions on a test dataset. It can also return class probabilities but it is disabled by default.
}
\examples{
\dontrun{
testing("model.rds", test_data)
testing("model.rds", test_data, return_probs = TRUE)
}
}
