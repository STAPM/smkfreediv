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
#' @param tot_duty_fm Numeric. Total duty receipts from factory made cigarettes
#' @param price_ryo Numeric. Price of 100g of hand-rolling tobacco.
#' @param duty_ryo Numeric. Duty per 1kg of hand-rolling tobacco.
#' @param tot_duty_ryo Numeric. Total duty receipts from hand-rolling tobacco.
#' @param deflate Numeric vector with 2 arguments containing the month and year corresponding to
#' the price of hand-rolled tobacco. This is to construct a deflation factor to Dec 2018 prices.
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
                        tot_duty_fm = 7748,
                        price_ryo = 51.60,
                        duty_ryo = 234.65,
                        tot_duty_ryo = 1444,
                        deflate = c(12,2020)) {

  #################################
  # Total Spending Calc - Toolkit #

# calculate mean weekly expenditure by local authority
exp <- smkfreediv::CalcWeekSpend(data = data,
                                 strat_vars = c("UTLAcode","UTLAname"),
                                 upshift = 1)

# merge in tobacco profiles
merge <- merge(exp, smkfreediv::PHE_tobacco_profiles, by = c("UTLAcode","UTLAname"))

# calculate total expenditure

merge[,tot_weekly_exp := n_smokers*mean_week_spend]
merge[,tot_annual_exp := n_smokers*mean_week_spend*52.25]

merge <- merge[,c("UTLAname","UTLAcode","n_smokers",
                  "mean_week_spend","tot_weekly_exp","tot_annual_exp")]

total_annual_spend <- sum(merge$tot_annual_exp, na.rm = TRUE)

##############################
# Total Spending Calc - HMRC #

## FM Cigs

total_excise_per_pack <- avt_fm*price_fm + (duty_fm/50)

excise_pct_fm <- total_excise_per_pack/price_fm

tot_spend_fm <- round(tot_duty_fm/excise_pct_fm)

## RYO tob

# deflation factor for prices
frm_yr <- as.numeric(smkfreediv::price_cigs[month == deflate[1] & year == deflate[2]],"price")
frm_yr <- frm_yr[1]
to_yr <- as.numeric(smkfreediv::price_cigs[month == 12 & year == 2018,"price"])

deflator <- to_yr/frm_yr

total_excise_per_100g <- (duty_ryo/10)

excise_pct_ryo <- total_excise_per_100g/(price_ryo*deflator)

tot_spend_ryo <- round(tot_duty_ryo/excise_pct_ryo)

## sum up

total_annual_spend_hmrc <- tot_spend_fm + tot_spend_ryo

######################################
## CALCULATE THE UPSHIFT MULTIPLIER ##

if (LCFS == TRUE) {
  upshift <- total_annual_spend_hmrc/5522
  total_annual_spend_surv <- 5522

} else {
  upshift <- total_annual_spend_hmrc/(total_annual_spend/1000000)
  total_annual_spend_surv <- (total_annual_spend/1000000)
}

return(list(upshift = upshift,
            tot_duty_fm = tot_duty_fm,
            price_fm = price_fm,
            duty_fm = duty_fm/50,
            avt  = avt_fm*price_fm,
            total_excise_per_pack = total_excise_per_pack,
            excise_pct_fm = excise_pct_fm,
            tot_spend_fm = tot_spend_fm,
            tot_duty_ryo = tot_duty_ryo,
            price_ryo = price_ryo,
            price_ryo_d = price_ryo*deflator,
            duty_ryo = duty_ryo/10,
            excise_pct_ryo = excise_pct_ryo,
            tot_spend_ryo = tot_spend_ryo,
            total_annual_spend_hmrc = total_annual_spend_hmrc,
            total_annual_spend_surv = total_annual_spend_surv
            ))
}

