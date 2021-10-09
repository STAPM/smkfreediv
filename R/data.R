#' CPI Index for Tobacco
#'
#' Consumer price index time series for tobacco products.
#'
#'
#' @format
#' @source \href{https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7cb/mm23}{Tobacco CPI time series}
"cpi_tobacco"

#' Local Authority Data.
#'
#' A dataset containing lower tier local authorities income and population counts. Raw data gives
#' model-based income estimates at Middle layer Super Output Area (MSOA) level, which are aggregated to lower-tier
#' local authority level.
#'
#' @format
#' @source \href{https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales}{Income Data}
"income"

#' Local Authority to Region Lookup
#'
#' A lookup table to match local authority districts to government office region geographies.
#'
#'
"la_gor_lookup"

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
#' A dataset containing smoking prevalence figures for 151 upper tier local authorities in
#' England, produced by Public Health England. Figures include smoking prevalence and
#' total number of smokers, and standard errors for both.
#'
#' @format
#' @source \href{https://fingertips.phe.org.uk/profile/tobacco-control/data#page/0}{PHE local tobacco profiles}
"PHE_tobacco_profiles"

#' PHW Tobacco Profiles.
#'
#' A dataset containing smoking prevalence figures for the 22 local authorities in
#' Wales, produced by Public Health Wales. Figures include smoking prevalence and
#' total number of smokers, and standard errors for both.
#'
#' @format
#' @source \href{https://publichealthwales.shinyapps.io/smokinginwales/}{PHW local tobacco profiles}
"PHW_tobacco_profiles"

#' Population.
#'
#' Mid-2018 ONS population estimates by age and sex.
#'
#' @format
#' @source \href{https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland}{Population Data by age, gender, local authority}
"pop_counts"

#' Price of Cigarettes
#'
#' Monthly time series of the price of a pack of 20 cigarettes from the ONS
#'
#' @format
#' @source \href{https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/czmp}{ONS Cigarette prices}
"price_cigs"

#' Proportion of English Smokers
#'
#' A scalar proportion of total UK smokers that are English. This is used to adjust UK total duty receipts from tobacco
#' to a figure representative for England, in order to feed into the calculation of estimated total expenditure on tobacco
#' in England for the upshift factor to apply to the toolkit data. This approach assumes that spending follows similar patterns
#' across the four UK nations such that expenditures are distributed the same way as are smokers.
#'
#' The proportion is calculated using data on smoking status collected in the Annual Population Survey (APS) from 2011 to 2018.
#' The proportion of UK smokers who are English is calculated separately for each year and averaged across the eight years. The
#' figure is stable at around 82% and statistically not significantly different year on year.
#'
#' @format
"prop_smokers_ENG"


#' Tobacco Duty Receipts
#'
#' Annual time series of the total duty receipts from factory-made cigarettes and hand-rolled tobacco.
#' Data by calendar year from 1992 to 2020.
#'
#' @format
#' @source \href{https://www.gov.uk/government/statistics/tobacco-bulletin}{ONS Tobacco Bulletin}
"tobacco_duty_receipts"

#' Upper Tier Local Authority to Region Lookup
#'
#' A lookup table to match local authority districts to government office region geographies.
#'
#'
"utla_gor_lookup"
