#' Calculate Local Authority Level Dividend
#'
#' Calculate the smoke-free dividend for each local authority.
#'
#' @param profiles PHE local tobacco profiles.
#' @param clean_income Data table. Cleaned income data.
#' @param clean_expenditure Data table. Cleaned local authority level weekly expenditure.
#' @param upshift Numeric. Parameter to upshift expenditures calculated from the toolkit data.
#' @param div Numeric. Proportion of expenditure paid in tax.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
CalcDividend_la <- function(profiles = smkfreediv::tobacco_profiles,
                            clean_income ,
                            clean_expenditure ,
                            upshift = 1.57151042,
                            div = 0.93) {

  ## grab the tobacco profiles and merge to mean expenditure

  # make Buckinghamshire match between the two datasets
  profiles[UTLAname == "Buckinghamshire UA", UTLAname := "Buckinghamshire"]
  clean_expenditure[UTLAcode == "E10000002", UTLAcode := "E06000060"]

  # fix mis-match of Bristol naming
  profiles[UTLAcode == "E06000023", UTLAname := "Bristol"]
  clean_expenditure[UTLAcode == "E06000023", UTLAname := "Bristol"]

  # fix mis-match of Kingston upon Hull naming
  profiles[UTLAcode == "E06000010", UTLAname := "Kingston upon Hull"]
  clean_expenditure[UTLAcode == "E06000010", UTLAname := "Kingston upon Hull"]

  # fix mis-match of Herefordshire naming
  profiles[UTLAcode == "E06000019", UTLAname := "Herefordshire"]
  clean_expenditure[UTLAcode == "E06000019", UTLAname := "Herefordshire"]

  merge <- merge(profiles, clean_expenditure, by = c("UTLAcode","UTLAname"), all = T)

  merge <- merge[order(UTLAname),]

  ### merge the income data to the two merged datasets

  # make Buckinghamshire match between the two datasets
  clean_income[UTLAname == "Buckinghamshire UA", UTLAname := "Buckinghamshire"]
  clean_income[UTLAcode == "E10000002", UTLAcode := "E06000060"]

  # fix mis-match of Bristol naming
  clean_income[UTLAcode == "E06000023", UTLAname := "Bristol"]

  # fix mis-match of Kingston upon Hull naming
  clean_income[UTLAcode == "E06000010", UTLAname := "Kingston upon Hull"]

  # fix mis-match of Herefordshire naming
  clean_income[UTLAcode == "E06000019", UTLAname := "Herefordshire"]

  merged_all <- merge(merge, clean_income, by = c("UTLAcode","UTLAname"), all = T)

  merged_all <- merged_all[order(UTLAname),]

  #################################################################
  ## Deterministic - calculate upshifting spending and dividend ###

  det <- copy(merged_all)

  det[, mean_week_spend_up := mean_week_spend * upshift]

  det[, total_wk_exp := round((n_smokers * mean_week_spend)/1000)]
  det[, total_annual_exp := round((n_smokers * mean_week_spend * 52)/1000000)]

  det[, spend_prop := (mean_week_spend * 52)/income]

  det[, mean_week_spend_up := mean_week_spend * upshift]
  det[, total_wk_exp_up := round((n_smokers * mean_week_spend * upshift)/1000)]
  det[, total_annual_exp_up := round((n_smokers * mean_week_spend * 52  * upshift)/1000000)]

  det[, spend_prop_up := (mean_week_spend_up * 52)/income]

  det[, dividend := total_annual_exp_up * div]


  det[, c("smk_prev_uci", "smk_prev_lci", "smk_prev_se","income_sim",
          "n_smokers_uci","n_smokers_lci", "n_smokers_se",
          "se_week_spend", "population", "sample_tkit") := NULL]

  ##################################################################
  ## Probabilistic - draw parameters from normal distributions #####

  prob <- copy(merged_all)

  ## generate probabilistic variables for
  # 1) number of smokers
  prob[!is.na(n_smokers),
       prob_n_smokers := rnorm(1,mean = n_smokers, sd = n_smokers_se) ,
       by = "UTLAcode"]

  # 2) smoking prevalence
  prob[!is.na(smk_prev),
       prob_smk_prev := rnorm(1,mean = smk_prev, sd = smk_prev_se) ,
       by = "UTLAcode"]

  # 3) mean weekly spend
  # small hack - if NA standard deviation for only one observations, set SE = MEAN/1.96
  #            - this increases standard deviation by as much as possible without allowing
  #              the lower confidence interval to fall below 0 (affects Bracknell Forest)
  prob[!is.na(mean_week_spend) & is.na(se_week_spend), se_week_spend := mean_week_spend/1.96]
  prob[!is.na(mean_week_spend),
       prob_mean_week_spend := rnorm(1,mean = mean_week_spend, sd = se_week_spend) ,
       by = "UTLAcode"]

  # 4) income
  prob[, prob_income := income_sim]

  ### repeat upshifting/dividend calculations with the
  ### probabilistically drawn values

  prob[, prob_mean_week_spend_up := prob_mean_week_spend * upshift]

  prob[, prob_total_wk_exp := round((prob_n_smokers * prob_mean_week_spend)/1000)]
  prob[, prob_total_annual_exp := round((prob_n_smokers * prob_mean_week_spend * 52)/1000000)]

  prob[, prob_spend_prop := (prob_mean_week_spend * 52)/prob_income]

  prob[, prob_mean_week_spend_up := prob_mean_week_spend * upshift]
  prob[, prob_total_wk_exp_up := round((prob_n_smokers * prob_mean_week_spend * upshift)/1000)]
  prob[, prob_total_annual_exp_up := round((prob_n_smokers * prob_mean_week_spend * 52  * upshift)/1000000)]

  prob[, prob_spend_prop_up := (prob_mean_week_spend_up * 52)/income]

  prob[, prob_dividend := prob_total_annual_exp_up * div]

  prob[, c("smk_prev", "smk_prev_uci", "smk_prev_lci", "smk_prev_se",
           "n_smokers", "n_smokers_uci","n_smokers_lci", "n_smokers_se",
           "se_week_spend", "mean_week_spend","sample_tkit",
           "pop_n", "population", "income_sim", "income") := NULL]

  ### merge

  data_out <- merge(det, prob, by = c("UTLAcode","UTLAname"))

  return(data_out)
}
