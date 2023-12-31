% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalcDividend_gor.R
\name{CalcDividend_gor}
\alias{CalcDividend_gor}
\title{Calculate Government Office Region Level Dividend}
\usage{
CalcDividend_gor(
  profiles = smkfreediv::PHE_tobacco_profiles,
  clean_income,
  clean_expenditure,
  div = 0.93,
  prob = FALSE,
  illicit_prop = 1298/(14307 + 1298)
)
}
\arguments{
\item{profiles}{PHE local tobacco profiles.}

\item{clean_income}{Data table. Cleaned income data.}

\item{clean_expenditure}{Data table. Cleaned local authority level weekly expenditure.}

\item{div}{Numeric. Proportion of expenditure paid in tax.}

\item{prob}{Character. If TRUE draws inputs to the dividend calculation probabilistically.}

\item{illicit_prop}{Numeric. The proportion of total tobacco expenditure which is illicit.}
}
\description{
Calculate the smoke-free dividend for each local authority.
}
\examples{

\dontrun{

}
}
