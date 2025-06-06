% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning.R
\name{get_targets}
\alias{get_targets}
\title{Get Unique Target Classes}
\usage{
get_targets(data, target)
}
\arguments{
\item{data}{A dataframe with target column}

\item{target}{The target column}
}
\value{
A dataframe with unique values of the target column
}
\description{
Get Unique Target Classes
}
\examples{
df <- data.frame(RiskLevel = c("low risk", "mid risk", "low risk", "high risk"))
get_targets(df, RiskLevel)
#>   RiskLevel
#> 1 low risk
#> 2 mid risk
#> 3 high risk

\dontrun{
get_targets(df, "RiskLevel")
get_targets(RiskLevel, df)
get_targets(df["RiskLevel"])
}

}
