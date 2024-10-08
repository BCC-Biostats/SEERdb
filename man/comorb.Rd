% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comorb.R
\name{comorb}
\alias{comorb}
\title{Get Comorbidity Information}
\usage{
comorb(
  infile,
  id,
  start_date,
  end_date,
  claim_start_date,
  claim_end_date,
  claim_type,
  dx_var_list,
  ruleout = FALSE
)
}
\arguments{
\item{infile}{A data frame containing claim records.}

\item{id}{A character string specifying the column name for patient IDs.}

\item{start_date}{A character string specifying the column name for the start date of the observation window.}

\item{end_date}{A character string specifying the column name for the end date of the observation window.}

\item{claim_start_date}{A character string specifying the column name for the claim start date.}

\item{claim_end_date}{A character string specifying the column name for the claim end date.}

\item{claim_type}{A character string specifying the column name for the type of claim (e.g., inpatient, outpatient).}

\item{dx_var_list}{A character vector specifying the column names for diagnosis codes.}

\item{ruleout}{Logical, whether to apply a ruleout window (default is FALSE).}
}
\value{
A data frame containing the comorbidity information, including the Charlson and NCI comorbidity indices.
}
\description{
This function processes claim data to identify comorbidities based on ICD-9 and ICD-10 diagnosis codes, and computes Charlson and NCI comorbidity indices.
}
\examples{
# Example usage with mock data
df <- data.frame(
  ID = c(1, 2),
  start_date = as.Date(c("2022-01-01", "2022-01-01")),
  end_date = as.Date(c("2022-12-31", "2022-12-31")),
  claim_start_date = as.Date(c("2022-06-01", "2022-08-01")),
  claim_end_date = as.Date(c("2022-06-10", "2022-08-10")),
  claim_type = c("M", "M"),
  dx1 = c("410", "I21"),
  dx2 = c(NA, "I252")
)
result <- comorb(df, id = "ID", start_date = "start_date", end_date = "end_date",
                claim_start_date = "claim_start_date", claim_end_date = "claim_end_date",
                claim_type = "claim_type", dx_var_list = c("dx1", "dx2"))
}
