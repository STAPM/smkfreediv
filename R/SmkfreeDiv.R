#' Calculate Smoke-free Dividend
#'
#' Calculate the smoke-free dividend at upper tier local authority (UTLA) level based on input. Requires
#' a data table of UTLA tobacco profiles and a data table of UTLA mean weekly tobacco expenditure calculated
#' from toolkit data
#'
#' @param profiles Data table. local tobacco profiles for 151 UTLAs.
#' @param expenditure Data table. local authority level mean weekly tobacco expenditure.
#' @param upshift Numeric. Upshift factor to apply to mean weekly tobacco expenditure. Default = 1.423
#' @param div Numeric. Proportion of licit expenditure paid in tax or industry profit. Default = 0.93
#' @param illicit_prop Numeric. The proportion of total tobacco expenditure which is illicit. Default = 0.083
#' @param n_sim Numeric. Number of simulation replications
#' @param seed Numeric. Random number seed for the simulation
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
SmkfreeDiv <- function(profiles = smkfreediv::PHE_tobacco_profiles,
                       expenditure ,
                       upshift = 1.423,
                       div = 0.93,
                       illicit_prop = 0.083,
                       n_sim = 10,
                       seed = 2021){

  set.seed(seed)

  #################################################
  ## merge tobacco profiles to mean expenditure ###

  merge <- merge(profiles, expenditure, by = c("UTLAcode"), all = T)

  merge <- merge[order(UTLAname),]

  #########################
  ## Simulate Dividend Calculation

  for (i in 1:n_sim) {

    cat("\t\tSimulating...", round(100*i/n_sim,2),"%", "               \r")
    utils::flush.console()
    if(i == n_sim) { cat("\n") }

    prob <- copy(merge)

    ## generate probabilistic variables for
    # 1) smoking prevalence
    prob[!is.na(smk_prev),
         prob_smk_prev := rnorm(1,mean = smk_prev, sd = smk_prev_se) ,
         by = "UTLAcode"]

    # 2) number of smokers
    prob[!is.na(n_smokers),
         prob_n_smokers := rnorm(1,mean = n_smokers, sd = n_smokers_se) ,
         by = "UTLAcode"]

    # 3) mean weekly spend
    # - if NA standard deviation for only one observations, set SE = MEAN/1.96
    # - this increases standard deviation by as much as possible without allowing
    #   the lower confidence interval to fall below 0


    prob[!is.na(mean_week_spend) & is.na(se_week_spend), se_week_spend := mean_week_spend/1.96]
    prob[!is.na(mean_week_spend),
         prob_mean_week_spend := rnorm(1,mean = mean_week_spend, sd = se_week_spend) ,
         by = "UTLAcode"]

    ### Use simulated values to calculate the dividend

    prob[, prob_total_wk_exp := round((prob_n_smokers * prob_mean_week_spend * upshift)/1000)]
    prob[, prob_total_annual_exp := round((prob_n_smokers * prob_mean_week_spend * upshift * 52)/1000000, 3)]

    prob[, prob_dividend := (prob_total_annual_exp * illicit_prop) + (prob_total_annual_exp * (1-illicit_prop))*div]


    ### retain variables needed

    prob <- prob[, c("UTLAname", "UTLAcode", "gor",
                     "pop_n", "prob_smk_prev", "prob_n_smokers", "prob_mean_week_spend",
                     "prob_total_wk_exp", "prob_total_annual_exp", "prob_dividend")]

    prob[, nsim := i]


    if (i == 1){

      data_out <- copy(prob)


    } else {

      data_out <- rbindlist(list(data_out, prob))
    }
  }

  ######################################
  ## Collapse to local authority level

  output_la <- data_out[, .(smkfreediv = mean(prob_dividend, na.rm = T) ,
                            smkfreediv_se = sd(prob_dividend, na.rm = T)), by = c("UTLAname","UTLAcode")]

  ######################################
  ## Collapse to government office region level

  ## first aggregate to gor level within simulations

  output_gor <- data_out[, .(prob_dividend = sum(prob_dividend, na.rm = T)), by = c("gor","nsim")]

  output_gor <- output_gor[, .(smkfreediv = mean(prob_dividend, na.rm = T) ,
                               smkfreediv_se = sd(prob_dividend, na.rm = T)), by = c("gor")]

  ######################################
  ## Collapse to national level

  ## first aggregate to national level within simulations

  output_nat <- data_out[, .(prob_dividend = sum(prob_dividend, na.rm = T)), by = c("nsim")]

  output_nat <- output_nat[, .(smkfreediv = mean(prob_dividend, na.rm = T) ,
                               smkfreediv_se = sd(prob_dividend, na.rm = T))]

  return(list(output_nat = output_nat,
              output_gor = output_gor,
              output_la = output_la))
}
