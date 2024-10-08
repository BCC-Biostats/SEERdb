% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_hsp_data.R
\name{get_hsp_data}
\alias{get_hsp_data}
\title{Create hsp dataset}
\usage{
get_hsp_data(
  db_directory = ".",
  hsp_base_files = NULL,
  hsp_condition_files = NULL,
  hsp_occurence_files = NULL,
  hsp_revenue_files = NULL,
  hsp_span_files = NULL,
  hsp_value_files = NULL,
  hsp_demo_files = NULL
)
}
\arguments{
\item{db_directory}{path to directory where data files are held}

\item{hsp_base_files}{vector of names of hsp.base files}

\item{hsp_condition_files}{vector of names of hsp.condition files}

\item{hsp_occurence_files}{vector of names of hsp.occurence files}

\item{hsp_revenue_files}{vector of names of hsp.revenue files}

\item{hsp_span_files}{vector of names of hsp.span files}

\item{hsp_value_files}{vector of names of hsp.value files}

\item{hsp_demo_files}{vector of names of hsp.demo files}
}
\value{
a list of dataframes from each given hsp file
}
\description{
Function will identify all of the hospice claims datasets in your directory and create a dataset for them.
There are 6 segments provided: base, revenue, condition, occurence, span, and value
}
\details{
PATIENT_ID and CLM_ID should be used for merging files
}
\examples{
hospitalDB <- SEERdb::get_hospital_data("../../master data files - SEER Medicare 2024/")


hospitalDB <- SEERdb::get_hospital_data(
                "../../master data files - SEER Medicare 2024/",
                c("hospital1996.withzip.txt.gz", "hospital2000.withzip.txt.gz")
                )

}
