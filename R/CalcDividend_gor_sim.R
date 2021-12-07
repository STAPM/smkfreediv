#' Simulate Government Office Region Dividend Calculation
#'
#' Calculate the smoke-free dividend for each local authority, using simulation
#' methods to incorporate parameter uncertainty in weekly expenditure, local smoking
#' prevalence, local authority average incomes.
#'
#' @param data Data table. Cleaned toolkit data
#' @param upshift Numeric. Parameter to upshift expenditures calculated from the toolkit data.
#' @param div Numeric. Proportion of expenditure paid in tax.
#' @param n_sim Numeric. Number of simulations.
#' @param seed Numeric. Random number seed.
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
CalcDividend_gor_sim <- function(data,
                                 upshift = 1.57151042,
                                 div = 0.93,
                                 n_sim = 100,
                                 seed = 2021,
                                 illicit_prop = 1298 / (14307 + 1298)) {

  set.seed(seed)

  ## Calculate mean weekly expenditure by upper-tier local authority
  cat(crayon::green("Calculating Regional Smokefree Dividends\n"))

  exp <- smkfreediv::CalcWeekSpend(data = data,
                                   strat_vars = c("gor"),
                                   upshift = upshift)

  exp <- exp[order(gor)]
  exp[gor == "Yorkshire and The Humber", gor := "Yorkshire and the Humber"]

  ## Grab income data

  income <- smkfreediv::GetIncome(income_data = smkfreediv::income,
                                  income_var = 3)
  income <- merge(income, smkfreediv::PHE_tobacco_profiles[,c("gor","UTLAname")], by = "UTLAname")

  income <- income[, .(income = weighted.mean(income, w = population),
                                 population = sum(population)), by = "gor"]
  ######## DETERMINISTIC CALCULATION

  div_la_det <- CalcDividend_gor(profiles = smkfreediv::PHE_tobacco_profiles,
                                 clean_income = income,
                                 clean_expenditure = exp,
                                 div = div,
                                 prob = FALSE,
                                 illicit_prop = illicit_prop)

  ######## PROBABILISTIC CALCULATION

  ## Initialise matrices for probabilistic variables

  m_n_smokers           <- matrix(rep(NA,n_sim*9), ncol = n_sim)
  m_smk_prev            <- matrix(rep(NA,n_sim*9), ncol = n_sim)
  m_mean_week_spend     <- matrix(rep(NA,n_sim*9), ncol = n_sim)
  m_income              <- matrix(rep(NA,n_sim*9), ncol = n_sim)
  m_total_wk_exp        <- matrix(rep(NA,n_sim*9), ncol = n_sim)
  m_total_annual_exp    <- matrix(rep(NA,n_sim*9), ncol = n_sim)
  m_spend_prop          <- matrix(rep(NA,n_sim*9), ncol = n_sim)
  m_dividend            <- matrix(rep(NA,n_sim*9), ncol = n_sim)

  cat(crayon::blue("\tGenerating Probabilistic Variables\n"))

  for (i in 1:n_sim) {

    cat("\t\tSimulating...", round(100*i/n_sim,2),"%", "               \r")
    utils::flush.console()
    if(i == n_sim) { cat("\n") }

    ## Grab the income data, including a probabilistically drawn mean income

    income_prob <- smkfreediv::GetIncome(income_data = smkfreediv::income,
                                         income_var = 3)
    income_prob <- merge(income_prob, smkfreediv::utla_gor_lookup, by = "UTLAname")

    income_prob <- income_prob[, .(income_sim = weighted.mean(income_sim, w = population),
                                   population = sum(population)), by = "gor"]

    ## Use the income/spending to calculate the smoke free dividend for
    ## each local authority.

    div_la <- CalcDividend_gor(profiles = smkfreediv::PHE_tobacco_profiles,
                               clean_income = income_prob,
                               clean_expenditure = exp,
                               div = div,
                               prob = TRUE)

    ## save out probabilistic results

    m_income[,i]              <- as.vector(as.matrix(div_la[,"prob_income"]))
    m_n_smokers[,i]           <- as.vector(as.matrix(div_la[,"prob_n_smokers"]))
    m_smk_prev[,i]            <- as.vector(as.matrix(div_la[,"prob_smk_prev"]))
    m_mean_week_spend[,i]     <- as.vector(as.matrix(div_la[,"prob_mean_week_spend"]))
    m_total_wk_exp[,i]        <- as.vector(as.matrix(div_la[,"prob_total_wk_exp"]))
    m_total_annual_exp[,i]    <- as.vector(as.matrix(div_la[,"prob_total_annual_exp"]))
    m_spend_prop[,i]          <- as.vector(as.matrix(div_la[,"prob_spend_prop"]))
    m_dividend[,i]            <- as.vector(as.matrix(div_la[,"prob_dividend"]))
  } # end simulation

  cat(crayon::blue("\tGenerating Simulation Means and Standard Deviations\n"))

  ### ---------------- (1) Income -------------------------------------###
  cat(crayon::cyan("\t\tAverage Annual Income"))

  m <- data.table(m_income)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_income   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_income, c("M","SD"), c("income_m","income_sd"))

  cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### ------------- (2) Number of Smokers -----------------------------###
  cat(crayon::cyan("\t\tNumber of Smokers"))

  m <- data.table(m_n_smokers)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_n_smokers   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_n_smokers, c("M","SD"), c("n_smokers_m","n_smokers_sd"))

  cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### -------------- (3) Smoking Prevalence ---------------------------###
  cat(crayon::cyan("\t\tSmoking Prevalence"))

  m <- data.table(m_smk_prev)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_smk_prev   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_smk_prev, c("M","SD"), c("smk_prev_m","smk_prev_sd"))

  cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### ----------- (4) Mean Weekly Spending ----------------------------###
  cat(crayon::cyan("\t\tMean Weekly Expenditure"))

  m <- data.table(m_mean_week_spend)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_mean_week_spend   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_mean_week_spend, c("M","SD"), c("mean_week_spend_m","mean_week_spend_sd"))

  cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### ----------- (5) Total weekly spending ---------------------------###
  cat(crayon::cyan("\t\tTotal Weekly Expenditure"))

  m <- data.table(m_total_wk_exp)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_total_wk_exp   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_total_wk_exp, c("M","SD"), c("total_wk_exp_m","total_wk_exp_sd"))

  cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### ----------- (6) Total annual spending ---------------------------###
  cat(crayon::cyan("\t\tTotal Annual Expenditure"))

  m <- data.table(m_total_annual_exp)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_total_annual_exp   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_total_annual_exp, c("M","SD"), c("total_annual_exp_m","total_annual_exp_sd"))

  cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### ----------- (7) Spending as % of Income -------------------------###
  cat(crayon::cyan("\t\tSpending proportion of Income"))

  m <- data.table(m_spend_prop)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_spend_prop   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_spend_prop, c("M","SD"), c("spend_prop_m","spend_prop_sd"))

  cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  ### ----------- (8) Smoke-free dividend -----------------------------###
  cat(crayon::cyan("\t\tSmokefree Dividend"))

  m <- data.table(m_dividend)

  m_m <- transform(m, M=apply(m,1, mean, na.rm = TRUE))
  m_s <- transform(m, SD=apply(m,1, sd, na.rm = TRUE))
  m_dividend   <- cbind(m_m[,"M"] ,m_s[,"SD"])

  setnames(m_dividend, c("M","SD"), c("dividend_m","dividend_sd"))

  cat("\t\tdone\n")
  ### -----------------------------------------------------------------###
  rm(m_m, m_s, m)
  gc()

  ### Combine deterministic/probabilistic data

  data_out <- cbind(div_la_det, m_income, m_n_smokers, m_smk_prev, m_mean_week_spend,
                    m_spend_prop, m_total_wk_exp, m_total_annual_exp)

  cat(crayon::green("done\n"))


  return(data_out)
}
