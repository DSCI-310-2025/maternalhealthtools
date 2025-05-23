% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning.R
\name{check_na}
\alias{check_na}
\title{Check Missing Values}
\usage{
check_na(data)
}
\arguments{
\item{data}{A dataframe}
}
\value{
A tibble of feature names and NA counts
}
\description{
Check Missing Values
}
\examples{
df <- data.frame(Age = c(25, NA, 35), BS = c(NA, NA, 8))
check_na(df)
#> # A tibble: 2 × 2
#>   feature     na
#>   <chr>    <int>
#>   Age          1
#>   BS           2

\dontrun{
check_na(100)
check_na(df['Age'])
check_na(df$Age)
}

}
