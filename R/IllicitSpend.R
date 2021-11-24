#' Calculate Illicit Spending
#'
#' Calculates the spending on illicit tobacco sources. First obtain the market share of
#' illicit tobacco consumption from HMRC tax gap tables for both factory-made and roll-your-own and obtain an
#' estimate for expenditure on illicit sources based on this percentage and the estimated expenditure
#' on duty-paid tobacco. These expenditures are then adjusted for the estimated
#' illicit prices as a fraction of the duty-paid prices (about a half).
#'
#' Prices are obtained from \href{https://www.ashscotland.org.uk/media/850413/28-calculating-the-cost-of-smoking-june-2021.pdf}{ASH Scotland},
#' for. The market share (consumption) figures are taken from the \href{https://www.gov.uk/government/statistics/tobacco-tax-gap-estimates}{tobacco tax gap estimates}.
#'
#' @param price_fm_paid Numeric. The price per cigarette of factory-made cigarettes.
#' @param price_fm_illicit Numeric. The price per cigarette of illicit factory-made cigarettes.
#' @param price_ryo_paid Numeric. The price per cigarette of roll-your-own tobacco.
#' @param price_ryo_illicit Numeric. The price per cigarette of illicit roll-your-own tobacco.
#' @param tot_legal_spend_fm Numeric. Estimate of total spending on duty-paid factory-made cigarettes.
#' @param tot_legal_spend_ryo Numeric. Estimate of total spending on duty-paid roll-your-own tobacco.
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
IllicitSpend <- function(price_fm_paid = 56.9,
                         price_fm_illicit = 28.5,
                         price_ryo_paid = 26,
                         price_ryo_illicit = 13,
                         tot_legal_spend_fm,
                         tot_legal_spend_ryo,
                         tax_gap_data = smkfreediv::tax_gap_data) {

  ### illicit price ratios (to adjust total expenditure on illicit by the
  ### price difference between illicit and duty-paid products)

  fm_ratio <- price_fm_illicit/price_fm_paid
  ryo_ratio <- price_ryo_illicit/price_ryo_paid

  ### obtain the consumption market shares for FM and RYO from the tax gap tables.

  share_illicit_fm = as.numeric(tax_gap_data[Source == "Illicit","share_fm"])
  share_legal_fm =   as.numeric(tax_gap_data[Source == "Legal","share_fm"])

  share_illicit_ryo = as.numeric(tax_gap_data[Source == "Illicit","share_ryo"])
  share_legal_ryo =   as.numeric(tax_gap_data[Source == "Legal","share_ryo"])

  ### Expenditure market share = consumption market share if the prices are the
  ### same. Otherwise, reduce the expenditure of illicit by the price ratio.
  ## calc illicit expenditures if price-ratio = 1 (i.e. expenditure share
  ## = consumption share)

  ## illicit spending = legal spending * (prop illegal / prop legal)

  temp_illicit_fm =  tot_legal_spend_fm*(share_illicit_fm/share_legal_fm)
  temp_illicit_ryo = tot_legal_spend_ryo*(share_illicit_ryo/share_legal_ryo)

  ## multiply by the price ratios

  tot_illicit_spend_fm <- temp_illicit_fm * fm_ratio
  tot_illicit_spend_ryo <- temp_illicit_ryo * ryo_ratio

  ## return the spending figures

  return(list(tot_illicit_spend_fm = tot_illicit_spend_fm,
              tot_illicit_spend_ryo = tot_illicit_spend_ryo,
              price_ratio_fm = fm_ratio,
              price_ratio_ryo = ryo_ratio,
              share_ratio_fm = share_illicit_fm/share_legal_fm,
              share_ratio_ryo = share_illicit_ryo/share_legal_ryo))
}
