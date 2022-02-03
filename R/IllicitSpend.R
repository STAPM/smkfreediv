#' Calculate Illicit Spending
#'
#' Calculates the spending on illicit tobacco sources. First obtain the market share of
#' illicit tobacco consumption from HMRC tax gap tables for both factory-made and roll-your-own and obtain an
#' estimate for expenditure on illicit sources based on this percentage and the estimated expenditure
#' on duty-paid tobacco. These expenditures are then adjusted for the estimated
#' illicit prices as a fraction of the duty-paid prices (about a half).
#'
#' Prices are obtained from \href{https://www.ashscotland.org.uk/media/850413/28-calculating-the-cost-of-smoking-june-2021.pdf}{ASH Scotland},
#' for. The market share (consumption) figures are taken from the \href{https://www.gov.uk/government/statistics/measuring-tax-gaps-tables}{measuring tax gaps tables 2021}.
#'
#' @param price_fm Numeric. Average price of legally sourced cigarettes.
#' @param price_ryo Numeric. Average price of legally sourced HRT/RYO tobacco.
#' @param price_fm_ratio Numeric. The illicit cigarette price as a proportion of legal prices.
#' @param price_ryo_ratio Numeric. The illicit HRT price as a proportion of legal prices.
#' @param illicit_data Data table. Consumption data for illicit sources of tobacco.
#' @param illicit_data_year Numeric. Year of illicit consumption data to use.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' }
IllicitSpend <- function(price_fm,
                         price_ryo,
                         price_fm_ratio = 28.5/56.9,
                         price_ryo_ratio = 13/26,
                         illicit_data = smkfreediv::tax_gap_data,
                         illicit_data_year = 2018) {

  ### obtain the consumption data for FM and RYO from the tax gap tables.

  data <- illicit_data[year == illicit_data_year,]

  ### calculate expenditure:

  ### FM: price is per 20 cigs, consumption is in billions of cigs
  ### RYO: price is per 100g of tobacco, consumption is in millions of kg

  # calculate consumption in packs of 20 sticks / 100g of tobacco
  # fm multiply by 1bn to get billions of sticks, then divide by 20 to get number of packs of 20
  # ryo multiply by 1bn (*1mn to get kilograms, *1000 to get in grams, /100 to get number of packs of 100g)

  cons_fm  <- as.numeric(data[,"illicit_volume_fm"])  * (1000000000/20)
  cons_ryo <- as.numeric(data[,"illicit_volume_ryo"]) * (1000000000/100)

  ### Expenditure = price * price ratio * consumption / 1000000 (to present in £millions)

  tot_illicit_spend_fm  <- (price_fm * price_fm_ratio * cons_fm)    / 1000000
  tot_illicit_spend_ryo <- (price_ryo * price_ryo_ratio * cons_ryo) / 1000000

  ## return the spending figures

  return(list(tot_illicit_spend_fm = tot_illicit_spend_fm,
              tot_illicit_spend_ryo = tot_illicit_spend_ryo,
              price_ratio_fm = price_fm_ratio,
              price_ratio_ryo = price_ryo_ratio,
              share_fm = as.numeric(data[,"illicit_volume_fm"]),
              share_ryo = as.numeric(data[,"illicit_volume_ryo"])
              ))

}
