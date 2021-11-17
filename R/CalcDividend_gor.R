#' Calculate Government Office Region Level Dividend
#'
#' Calculate the smoke-free dividend for each local authority.
#'
#' @param profiles PHE local tobacco profiles.
#' @param clean_income Data table. Cleaned income data.
#' @param clean_expenditure Data table. Cleaned local authority level weekly expenditure.
#' @param div Numeric. Proportion of expenditure paid in tax.
#' @param prob Character. If TRUE draws inputs to the dividend calculation probabilistically.
#' @param illicit_prop Numeric. The proportion of total tobacco expenditure which is illicit.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
CalcDividend_gor <- function(profiles = smkfreediv::PHE_tobacco_profiles,
                             clean_income ,
                             clean_expenditure ,
                             div = 0.93,
                             prob = FALSE,
                             illicit_prop = 1298 / (14307 + 1298)) {

  ## grab the tobacco profiles and merge to mean expenditure

  profiles = merge(profiles, smkfreediv::utla_gor_lookup, by = "UTLAname")

  profiles[!is.na(smk_prev), smk_prev_sim := rnorm(1,mean = smk_prev, sd = smk_prev_se), by = "UTLAname"]
  profiles[!is.na(n_smokers), n_smokers_sim := rnorm(1,mean = n_smokers, sd = n_smokers_se), by = "UTLAname"]

  profiles <- profiles[, .(smk_prev = weighted.mean(smk_prev, w = pop_n, na.rm = TRUE),
                           smk_prev_sim = weighted.mean(smk_prev_sim, w = pop_n, na.rm = TRUE),
                           n_smokers = sum(n_smokers, na.rm = TRUE),
                           n_smokers_sim = sum(n_smokers_sim, na.rm = TRUE),
                           pop_n = sum(pop_n, na.rm = TRUE)), by = "gor"]


  merge <- merge(profiles, clean_expenditure, by = c("gor"), all = T)

  merge <- merge[order(gor),]

  ### merge the income data to the two merged datasets

  merged_all <- merge(merge, clean_income, by = c("gor"), all = T)

  merged_all <- merged_all[order(gor),]

  if (prob == FALSE) {
    #################################################################
    ## Deterministic - calculate upshifting spending and dividend ###

    det <- copy(merged_all)

    det[, total_wk_exp := round((n_smokers * mean_week_spend)/1000)]
    det[, total_annual_exp := round((n_smokers * mean_week_spend * 52)/1000000, 3)]

    det[, spend_prop := (mean_week_spend * 52)/income]

    det[, dividend := (total_annual_exp * illicit_prop) + (total_annual_exp * (1-illicit_prop))*div]

    det[, c("se_week_spend", "population", "sample_tkit") := NULL]
    #"n_smokers_se","smk_prev_se","income_sim",

    data_out <- copy(det)
  } else if (prob == TRUE) {

    ##################################################################
    ## Probabilistic - draw parameters from normal distributions #####

    prob <- copy(merged_all)

    ## generate probabilistic variables for
    # 1) smoking prevalence
    prob[,prob_smk_prev := smk_prev_sim]

    # 2) number of smokers
    prob[,prob_n_smokers := n_smokers_sim]

    # 3) mean weekly spend
    # small hack - if NA standard deviation for only one observations, set SE = MEAN/1.96
    #            - this increases standard deviation by as much as possible without allowing
    #              the lower confidence interval to fall below 0 (affects Bracknell Forest)
    prob[!is.na(mean_week_spend) & is.na(se_week_spend), se_week_spend := mean_week_spend/1.96]
    prob[!is.na(mean_week_spend),
         prob_mean_week_spend := rnorm(1,mean = mean_week_spend, sd = se_week_spend) ,
         by = "gor"]

    # 4) income
    prob[, prob_income := income_sim]

    ### repeat upshifting/dividend calculations with the
    ### probabilistically drawn values

    prob[, prob_total_wk_exp := round((prob_n_smokers * prob_mean_week_spend)/1000)]
    prob[, prob_total_annual_exp := round((prob_n_smokers * prob_mean_week_spend * 52)/1000000, 3)]

    prob[, prob_spend_prop := (prob_mean_week_spend * 52)/prob_income]

    prob[, prob_dividend := (prob_total_annual_exp * illicit_prop) + (prob_total_annual_exp * (1-illicit_prop))*div]

    prob[, c("smk_prev", "n_smokers",
             "se_week_spend", "mean_week_spend","sample_tkit",
             "pop_n", "population", "income_sim") := NULL]
    #"n_smokers_se",, "smk_prev_se", "income"
    data_out <- copy(prob)
  }

  return(data_out)
}
