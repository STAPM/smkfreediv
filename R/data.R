#' CPI Index for Tobacco
#'
#' Consumer price index time series for tobacco products.
#'
#'
#' @format
#' @source \href{https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7cb/mm23}{Tobacco CPI time series}
"cpi_tobacco"

#' Local Authority Income Data.
#'
#' A dataset containing lower tier local authorities income and population counts. Raw data gives
#' model-based income estimates at Middle layer Super Output Area (MSOA) level, which are aggregated to lower-tier
#' local authority level by simple mean averaging.
#'
#' @format
#' @source \href{https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales}{Income Data}
"income"

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
#' Mid-2019 ONS 18+ population estimates by local authority.
#'
#' @format
#' @source \href{https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland}{ONS Population Data}
"pop_counts"

#' Price of Cigarettes
#'
#' Monthly time series of the price of a pack of 20 cigarettes from the ONS
#'
#' @format
#' @source \href{https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/czmp}{ONS Cigarette prices}
"price_cigs"

#' Price of Cigarettes (OECD)
#'
#' Prices for a pack of 20 cigarettes by country, defined as the Weighted Average Price (WAP). This is the average consumer price
#' of a tobacco product based on the prices of individual brands and weighted by sales. This data table contains prices for 2016. For
#' the UK the WAP for a pack of 20 cigarettes is £8.30.
#'
#' @docType data
#'
#' @format A data table (drinker_cat, product, product2, strength, mktshare).
#'
#' \describe{
#'   \item{country}{}
#'   \item{price_usd_excl_tax}{The pre-tax price (measured in USD), including producer and distributor margins.}
#'   \item{duty_percent}{percentage of total retail price paid in specific duty.}
#'   \item{avt_percent}{percentage of total retail price paid in duty on value (or ad-valorem tax).}
#'   \item{vat_percent}{percentage of total retail price paid in value added tax (or equivalent, e.g. retail sales tax for the US).}
#'   \item{tax_percent}{percentage of total retail price paid in taxation.}
#'   \item{currency}{local currency.}
#'   \item{price}{retail sales price in the local currency.}
#'   \item{price_usd}{retail sales price in US dollars.}
#' }
#' @source
#' \itemize{
#' \item \href{https://www.oecd.org/tax/consumption/tax-burden-cigarettes-ctt-trends.xlsx}{OECD tax burden of cigarettes} sourced from the World Health Organisation.
#' }
"price_cigs_oecd"

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

#' Measuring Tax Gaps Data
#'
#' Data extract from the 2021 measuring tax gaps tables produced by HMRC. Contains estimates of the volume of illicit consumption of
#' cigarettes and hand-rolling tobacco from 2000-01 to 2019-20.
#'
#' @docType data
#'
#' @format
#'
#' \describe{
#'   \item{year}{year (calendar)}
#'   \item{tax_year}{year (tax)}
#'   \item{illicit_share_fm}{share of illicit products in total consumption of factory-made cigarettes.}
#'   \item{illicit_volume_fm}{volume of illicit cigarettes (billion sticks).}
#'   \item{illicit_share_ryo}{share of illicit products in total consumption of hand-rolling tobacco.}
#'   \item{illicit_volume_ryo}{volume of illicit cigarettes (million kg).}
#' }
#'
#' @source
#' \itemize{
#' \item \href{https://www.gov.uk/government/statistics/measuring-tax-gaps/measuring-tax-gaps-2021-edition-tax-gap-estimates-for-2019-to-2020}{Measuring tax gaps 2021 edition - tax gap estimates for 2019 to 2020}
#' \item \href{https://www.gov.uk/government/statistics/measuring-tax-gaps-tables}{Measuring tax gaps tables 2021}
#' }
"tax_gap_data"

#' Tobacco Duty Receipts
#'
#' Annual time series of the total duty receipts from factory-made cigarettes and hand-rolled tobacco.
#' Data by calendar year from 1992 to 2020.
#'
#' @format
#' @source \href{https://www.gov.uk/government/statistics/tobacco-bulletin}{ONS Tobacco Bulletin}
"tobacco_duty_receipts"

#' Processed Toolkit Data
#'
#' Toolkit data taken from the April 2014 - February 2020 waves to be used in calculating the
#' tobacco expenditure by local authority. Expenditures are deflated to December 2018 prices.
#'
#' Data includes variables on consumption and expenditure of tobacco products, upper tier local authority
#' and region indicators, and socioeconomic variables including age, sex, social grade, education,
#' and labour market status.
#'
#' @format
"TOOLKIT"

