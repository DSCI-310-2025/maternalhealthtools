# R/cleaning.R

#' Check Missing Values
#'
#' @param data A dataframe
#' @return A tibble of feature names and NA counts
#' @export
#'
#' @examples
#' df <- data.frame(Age = c(25, NA, 35), BS = c(NA, NA, 8))
#' check_na(df)
#' #> # A tibble: 2 × 2
#' #>   feature     na
#' #>   <chr>    <int>
#' #>   Age          1
#' #>   BS           2
#'
#' \dontrun{
#' check_na(100)
#' check_na(df['Age'])
#' check_na(df$Age)
#' }
#'
check_na <- function(data) {
  tibble::tibble(
    feature = names(data),
    na = colSums(is.na(data)))
}

#' Get Unique Target Classes
#'
#' @param data A dataframe with target column
#' @param target The target column
#' @return A dataframe with unique values of the target column
#' @export
#'
#' @examples
#' df <- data.frame(RiskLevel = c("low risk", "mid risk", "low risk", "high risk"))
#' get_targets(df, RiskLevel)
#' #>   RiskLevel
#' #> 1 low risk
#' #> 2 mid risk
#' #> 3 high risk
#'
#' \dontrun{
#' get_targets(df, "RiskLevel")
#' get_targets(RiskLevel, df)
#' get_targets(df["RiskLevel"])
#' }
#'
get_targets <- function(data, target) {
  data %>% dplyr::distinct({{ target }})
}

#' Get Unique Target Classes
#'
#' @param data A dataframe
#' @param target The target column
#' @return A cleaned dataframe with missing values removed and the categorical target variable converted to a factor
#' @export
#'
#' @examples
#' df <- data.frame(Age = c(25, 35, NA, 29), RiskLevel = c("high risk", NA, "low risk", "mid risk"))
#' clean(df, RiskLevel)
#' #>   Age    RiskLevel
#' #> 1   25    high risk
#' #> 2   29     mid risk
#'
#' \dontrun{
#' clean(df, "RiskLevel")
#' clean(RiskLevel, df)
#' clean(df["RiskLevel"])
#' }
#'
clean <- function(data, target) {
  target_col <- dplyr::pull(data, {{ target }})

  if (is.character(target_col)) {
    data <- data %>%
      dplyr::mutate({{ target }} := as.factor({{ target }}))
  }
  data %>% tidyr::drop_na() %>% dplyr::distinct()
}
