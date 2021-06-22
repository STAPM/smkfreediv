#' Local Authority to Region Lookup
#'
#' A lookup table to match local authority districts to government office region geographies.
#'
#' @format
"la_gor_lookup"

#' Local Authority Data.
#'
#' A dataset containing lower tier local authorities income and population counts. Raw data gives
#' model-based income estimates at Middle layer Super Output Area (MSOA) level, which are aggregated to lower-tier
#' local authority level.
#'
#' @format
#' @source \href{https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales}{Income Data}
"la_inc_pop"

#' Local Authority Names and Codes.
#'
#' A dataset containing the names and codes of lower tier local authorities
#' (LTLAs) and their mapping to upper tier local authorities (UTLAs) - the level at
#' which PHE tobacco profiles are published.
#'
#' @format
#' @source \href{https://data.gov.uk/dataset/6ee49b1e-0f4d-4079-90f4-b626e36d2035/lower-tier-local-authority-to-upper-tier-local-authority-april-2019-lookup-in-england-and-wales}{Local Authority lookup}
"localauthorities"

#' PHE Tobacco Profiles.
#'
#' A dataset containing smoking prevalence figures for upper tier local authorities in
#' England, produced by Public Health England. Figures include smoking prevalence and
#' total number of smokers, including 95% confidence intervals and standard errors for both.
#'
#' @format
#' @source \href{https://fingertips.phe.org.uk/profile/tobacco-control/data#page/0}{PHE local tobacco profiles}
"tobacco_profiles"
