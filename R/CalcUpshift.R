#' Calculate Upshift Factor
#'
#' As survey data tends to underestimate expenditure on tobacco products, this function
#' combines estimated total expenditure in the Smoking Toolkit Study data with estimates
#' based on average prices and HMRC duty receipts. The ratio of the total HMRC-based estimated
#' annual spend to the toolkit-based estimate is the upshift factor.
#'
#' Calculations are based on the 2018/19 tax year. Total annual expenditure is calculated by
#' estimating the % of total price of a pack of 20 cigarettes/100g of hand-rolling tobacco which is
#' made up of excise duty and using this to derive total spending from total duty receipts. The
#' corresponding figure from the Toolkit data is obtained by calculating mean weekly spend per smoker
#' by local authority, multiplying this by the number of smokers in the tobacco profiles, then scaling this
#' figure up to an annual figure and summing over local authorities.
#'
#' @param data Data table. The cleaned toolkit data.
#' @param LCFS Logical. If TRUE use the LCFS grossed-up spend estimated by Howard Reed.
#' @param price_fm Numeric. Price of factory made cigarettes per pack of 20.
#' @param duty_fm Numeric. Duty per 1,000 cigarettes
#' @param avt_fm Numeric. Ad-valorem tax rate on factory made cigarettes.
#' @param price_ryo Numeric. Price of 100g of hand-rolling tobacco.
#' @param duty_ryo Numeric. Duty per 1kg of hand-rolling tobacco.
#' @param deflate_from Numeric vector with 2 arguments containing the month and year corresponding to
#' the price of hand-rolled tobacco.
#' @param deflate_to Numeric vector with 2 arguments containing the month and year to which hand-rolled tobacco
#' prices should be deflated.
#' @param profiles_data Data table. PHE tobacco profiles.
#' @param receipts_data Data table. Tobacco duty receipts data.
#' @param prices_data Data table. Price data for a 20-pack of cigarettes.
#' @param adjust Logical. If TRUE (default) adjust total receipts by the proportion of smokers who are English. If FALSE
#' use total UK tobacco duty receipt figure.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Use the function defaults to obtain the upshift factor.
#'
#' up <- CalcUpshift(data = data)
#'
#' ## Set LCFS = TRUE. This combined with the other defaults returns
#' ## the estimate by Howard Reed
#'
#' up <- CalcUpshift(data = data,
#'                   LCFS = TRUE)
#'
#' }
CalcUpshift <- function(data = data,
                        LCFS = FALSE,
                        price_fm = 8.83,
                        duty_fm = 228.29,
                        avt_fm = 0.165,
                        price_ryo = 51.60,
                        duty_ryo = 234.65,
                        deflate_from = c(12,2020),
                        deflate_to = c(12,2018),
                        profiles_data = smkfreediv::PHE_tobacco_profiles,
                        receipts_data = smkfreediv::tobacco_duty_receipts,
                        prices_data = smkfreediv::price_cigs,
                        adjust = TRUE) {

  #################################
  # Total Spending Calc - Toolkit #

  # calculate mean weekly expenditure
  tot_mean_spend <- round( as.numeric( smkfreediv::CalcWeekSpend(data, strat_vars = NULL, upshift = 1)[,"mean_week_spend"] ), 2)

  # number of smokers
  tot_smokers    <- round( sum(profiles_data[, "n_smokers"], na.rm = T) )

  # calculate total weekly expenditure and total annual expenditure

  total_weekly_spend <- (tot_mean_spend * tot_smokers )/1000000
  total_annual_spend <- (tot_mean_spend * tot_smokers * 52)/1000000

  ##############################
  # Total Spending Calc - HMRC #

  ## grab total duty for both products from the same year as deflating prices to. adjust the
  ## total duties to an England-only figure by proportion of UK smokers who
  ## live in england (calculated from APS)

  tot_duty_fm  <- as.numeric(receipts_data[year == deflate_to[2],"FM_cigs"])
  tot_duty_ryo <- as.numeric(receipts_data[year == deflate_to[2],"RYO_tob"])

  if (adjust == TRUE) {
    tot_duty_fm  <- tot_duty_fm*smkfreediv::prop_smokers_ENG
    tot_duty_ryo <- tot_duty_ryo*smkfreediv::prop_smokers_ENG
  }

  ## FM Cigs ##
  # calculate excise duty paid per pack, as a percentage of price, use this
  # to uprate the duty receipts into total expenditures

  total_excise_per_pack <- avt_fm*price_fm + (duty_fm/50)

  excise_pct_fm <- total_excise_per_pack/price_fm

  tot_legal_spend_fm <- round(tot_duty_fm/excise_pct_fm)

  ## RYO tob
  # calculate excise duty paid per pack, as a percentage of price, use this
  # to uprate the duty receipts into total expenditures

  # deflation factor for RYO prices
  frm_yr <- as.numeric(prices_data[month == deflate_from[1] & year == deflate_from[2]],"price")
  frm_yr <- frm_yr[1]
  to_yr  <- as.numeric(prices_data[month == deflate_to[1] & year == deflate_to[2]],"price")
  to_yr  <- to_yr[1]

  deflator <- to_yr/frm_yr
  price_ryo_d <- price_ryo*deflator

  total_excise_per_100g <- (duty_ryo/10)

  excise_pct_ryo <- total_excise_per_100g/price_ryo_d

  tot_legal_spend_ryo <- round(tot_duty_ryo/excise_pct_ryo)

  ####################
  ## Calculate Illicit Spending

  illicit <- smkfreediv::IllicitSpend(tot_legal_spend_fm = tot_legal_spend_fm,
                                      tot_legal_spend_ryo = tot_legal_spend_ryo)

  ###############################
  ## sum up RYO and FM figures

  tot_spend_fm  <- tot_legal_spend_fm + illicit[["tot_illicit_spend_fm"]]
  tot_spend_ryo <- tot_legal_spend_ryo + illicit[["tot_illicit_spend_ryo"]]

  total_annual_spend_hmrc = tot_spend_fm + tot_spend_ryo

  illicit_prop <- (illicit[["tot_illicit_spend_fm"]] + illicit[["tot_illicit_spend_ryo"]]) / total_annual_spend_hmrc

  ## create the amount of illicit spending (Howard Reed calcs)

  #illicit_prop = 1298 / (14307 + 1298)
  #if (adjust == TRUE) {
  #illicit = (legal/(1 - illicit_prop)) - legal
  #} else if (adjust == FALSE) {
  #illicit = 0
  #}

  ######################################
  ## CALCULATE THE UPSHIFT MULTIPLIER ##

  if (LCFS == TRUE) {
    upshift <- total_annual_spend_hmrc/5522
    total_annual_spend_surv <- 5522
    source = "LCFS"

    } else {
    upshift <- total_annual_spend_hmrc/total_annual_spend
    total_annual_spend_surv <- total_annual_spend
    source = "Toolkit"

  }

return(list(upshift = upshift,
            tot_duty_fm = tot_duty_fm,
            price_fm = price_fm,
            duty_fm = duty_fm,
            duty_fm_pp = duty_fm/50,
            avt_rate = avt_fm,
            avt  = avt_fm*price_fm,
            total_excise_per_pack = total_excise_per_pack,
            excise_pct_fm = excise_pct_fm,
            tot_legal_spend_fm = tot_legal_spend_fm,
            tot_illicit_spend_fm = illicit[["tot_illicit_spend_fm"]],
            tot_duty_ryo = tot_duty_ryo,
            price_ryo = price_ryo,
            deflator = deflator,
            price_ryo_d = price_ryo_d,
            duty_ryo    = duty_ryo,
            duty_ryo_pp = duty_ryo/10,
            excise_pct_ryo = excise_pct_ryo,
            tot_legal_spend_ryo = tot_legal_spend_ryo,
            tot_illicit_spend_ryo = illicit[["tot_illicit_spend_ryo"]],
            illicit_prop = illicit_prop,
            total_annual_spend_hmrc = total_annual_spend_hmrc,
            total_annual_spend_surv = total_annual_spend_surv,
            svy_data = source
            ))
}
