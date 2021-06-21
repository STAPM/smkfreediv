#' Calculate Local Authority Level Dividend
#'
#' Calculate the smoke-free dividend for each local authority, using simulation
#' methods to incorporate parameter uncertainty in weekly expenditure, local smoking
#' prevalence, local authority average incomes.
#'
#' @param profiles PHE local tobacco profiles.
#' @param clean_income Data table. Cleaned income data.
#' @param clean_expenditure Data table. Cleaned local authority level weekly expenditure.
#' @param upshift Numeric. Parameter to upshift expenditures calculated from the toolkit data.
#' @param div Numeric. Proportion of expenditure paid in tax.
#' @param reps Integer. Number of simulation replications.
#' @param seed Integer. Random number seed.
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
                            div = 0.93,
                            reps = 10000,
                            seed = 2021) {

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

  det[, total_wk_exp := round((n_smokers * mean_week_spend)/1000)]
  det[, total_annual_exp := round((n_smokers * mean_week_spend * 52)/1000000)]

  det[, total_wk_exp_up := round((n_smokers * mean_week_spend * upshift)/1000)]
  det[, total_annual_exp_up := round((n_smokers * mean_week_spend * 52  * upshift)/1000000)]

  det[, dividend := total_annual_exp_up * div]

  det[, c("smk_prev_uci", "smk_prev_lci", "smk_prev_se",
          "n_smokers_uci","n_smokers_lci", "n_smokers_se",
          "se_week_spend", "population", "sample_tkit") := NULL]

  ##################################################################
  ## Probabilistic - draw parameters from normal distributions #####

  # initialise matrices to store the simulation results

  dim = dim(merged_all)[1]*reps

  m_mean_week_spend     = matrix(rep(NA, dim), ncol = reps)
  m_n_smokers           = matrix(rep(NA, dim), ncol = reps)
  m_dividend            = matrix(rep(NA, dim), ncol = reps)
  m_total_wk_exp        = matrix(rep(NA, dim), ncol = reps)
  m_total_annual_exp    = matrix(rep(NA, dim), ncol = reps)
  m_total_wk_exp_up     = matrix(rep(NA, dim), ncol = reps)
  m_total_annual_exp_up = matrix(rep(NA, dim), ncol = reps)

  cat(crayon::blue("\tSimulating Local Authority Calculations\n"))

  set.seed(seed)
  for (i in 1:reps) {

    # Simulating
    cat("\t\tReplication", i, "of", reps, "               \r")
    utils::flush.console()
    if(i == reps) { cat("\n") }

    prob <- copy(merged_all)

    ## generate probabilistic variables for
    # 1) number of smokers
    # 2) mean weekly spend
    # 3) income

    prob[!is.na(n_smokers),
         n_smokers_prob := rnorm(1,mean = n_smokers, sd = n_smokers_se) ,
         by = "UTLAcode"]

    # small hack - if NA standard deviation for only one observations, set SE = MEAN
    prob[!is.na(mean_week_spend) & is.na(se_week_spend), se_week_spend := mean_week_spend]
    prob[!is.na(mean_week_spend),
         mean_week_spend_prob := rnorm(1,mean = mean_week_spend, sd = se_week_spend) ,
         by = "UTLAcode"]

    ### repeat calculations but with the probabilistically drawn values

    prob[, total_wk_exp := round((n_smokers_prob * mean_week_spend_prob)/1000)]
    prob[, total_annual_exp := round((n_smokers_prob * mean_week_spend_prob * 52)/1000000)]

    prob[, total_wk_exp_up := round((n_smokers_prob * mean_week_spend_prob * upshift)/1000)]
    prob[, total_annual_exp_up := round((n_smokers_prob * mean_week_spend_prob * 52  * upshift)/1000000)]

    prob[, dividend := total_annual_exp_up * div]


    ### fill in the current results matrices columns

    m_mean_week_spend[, i]    <- as.matrix(as.vector(prob[, "mean_week_spend_prob"]))
    m_n_smokers[, i]          <- as.matrix(as.vector(prob[, "n_smokers_prob"]))
    m_total_wk_exp[,i]        <- as.matrix(as.vector(prob[, "total_wk_exp"]))
    m_total_annual_exp[,i]    <- as.matrix(as.vector(prob[, "total_annual_exp"]))
    m_total_wk_exp_up[,i]     <- as.matrix(as.vector(prob[, "total_wk_exp_up"]))
    m_total_annual_exp_up[,i] <- as.matrix(as.vector(prob[, "total_annual_exp_up"]))
    m_dividend[,i]            <- as.matrix(as.vector(prob[, "dividend"]))

    ### clean up
    rm(prob)
  }

  cat(crayon::blue("\tdone\n"))

  m_mean_week_spend     <- data.table(m_mean_week_spend)
  m_n_smokers           <- data.table(m_n_smokers)
  m_total_wk_exp        <- data.table(m_total_wk_exp)
  m_total_annual_exp    <- data.table(m_total_annual_exp)
  m_total_wk_exp_up     <- data.table(m_total_wk_exp_up)
  m_total_annual_exp_up <- data.table(m_total_annual_exp_up)
  m_dividend            <- data.table(m_dividend)

  ### from each results matrix, extract mean and standard deviations

  ## mean weekly spending
  m_mean_week_spend_mean <- transform(m_mean_week_spend, M=apply(m_mean_week_spend,1, mean, na.rm = TRUE))
  m_mean_week_spend_sd   <- transform(m_mean_week_spend, SD=apply(m_mean_week_spend,1, sd, na.rm = TRUE))
  m_mean_week_spend      <- cbind(m_mean_week_spend_mean[,"M"] ,m_mean_week_spend_sd[,"SD"])

  m_mean_week_spend <- data.table(m_mean_week_spend)
  setnames(m_mean_week_spend, c("M","SD"), c("mean_week_spend_mean","mean_week_spend_sd"))
  rm(m_mean_week_spend_mean, m_mean_week_spend_sd)

  ## total number of smokers
  m_n_smokers_mean <- transform(m_n_smokers, M=apply(m_n_smokers,1, mean, na.rm = TRUE))
  m_n_smokers_sd   <- transform(m_n_smokers, SD=apply(m_n_smokers,1, sd, na.rm = TRUE))
  m_n_smokers      <- cbind(m_n_smokers_mean[,"M"] ,m_n_smokers_sd[,"SD"])

  m_n_smokers <- data.table(m_n_smokers)
  setnames(m_n_smokers, c("M","SD"), c("n_smokers_mean","n_smokers_sd"))
  rm(m_n_smokers_mean, m_n_smokers_sd)

  ## total weekly expenditure
  m_total_wk_exp_mean <- transform(m_total_wk_exp, M=apply(m_total_wk_exp,1, mean, na.rm = TRUE))
  m_total_wk_exp_sd   <- transform(m_total_wk_exp, SD=apply(m_total_wk_exp,1, sd, na.rm = TRUE))
  m_total_wk_exp      <- cbind(m_total_wk_exp_mean[,"M"] ,m_total_wk_exp_sd[,"SD"])

  m_total_wk_exp <- data.table(m_total_wk_exp)
  setnames(m_total_wk_exp, c("M","SD"), c("total_wk_exp_mean","total_wk_exp_sd"))
  rm(m_total_wk_exp_mean, m_total_wk_exp_sd)

  ## total annual expenditure
  m_total_annual_exp_mean <- transform(m_total_annual_exp, M=apply(m_total_annual_exp,1, mean, na.rm = TRUE))
  m_total_annual_exp_sd   <- transform(m_total_annual_exp, SD=apply(m_total_annual_exp,1, sd, na.rm = TRUE))
  m_total_annual_exp      <- cbind(m_total_annual_exp_mean[,"M"] ,m_total_annual_exp_sd[,"SD"])

  m_total_annual_exp <- data.table(m_total_annual_exp)
  setnames(m_total_annual_exp, c("M","SD"), c("total_annual_exp_mean","total_annual_exp_sd"))
  rm(m_total_annual_exp_mean, m_total_annual_exp_sd)

  ## total upshifted weekly expenditure
  m_total_wk_exp_up_mean <- transform(m_total_wk_exp_up, M=apply(m_total_wk_exp_up,1, mean, na.rm = TRUE))
  m_total_wk_exp_up_sd   <- transform(m_total_wk_exp_up, SD=apply(m_total_wk_exp_up,1, sd, na.rm = TRUE))
  m_total_wk_exp_up      <- cbind(m_total_wk_exp_up_mean[,"M"] ,m_total_wk_exp_up_sd[,"SD"])

  m_total_wk_exp_up <- data.table(m_total_wk_exp_up)
  setnames(m_total_wk_exp_up, c("M","SD"), c("total_wk_exp_up_mean","total_wk_exp_up_sd"))
  rm(m_total_wk_exp_up_mean, m_total_wk_exp_up_sd)

  ## total upshifted annual expenditure
  m_total_annual_exp_up_mean <- transform(m_total_annual_exp_up, M=apply(m_total_annual_exp_up,1, mean, na.rm = TRUE))
  m_total_annual_exp_up_sd   <- transform(m_total_annual_exp_up, SD=apply(m_total_annual_exp_up,1, sd, na.rm = TRUE))
  m_total_annual_exp_up      <- cbind(m_total_annual_exp_up_mean[,"M"] ,m_total_annual_exp_up_sd[,"SD"])

  m_total_annual_exp_up <- data.table(m_total_annual_exp_up)
  setnames(m_total_annual_exp_up, c("M","SD"), c("total_annual_exp_up_mean","total_annual_exp_up_sd"))
  rm(m_total_annual_exp_up_mean, m_total_annual_exp_up_sd)

  ## dividend
  m_dividend_mean <- transform(m_dividend, M=apply(m_dividend,1, mean, na.rm = TRUE))
  m_dividend_sd   <- transform(m_dividend, SD=apply(m_dividend,1, sd, na.rm = TRUE))
  m_dividend      <- cbind(m_dividend_mean[,"M"] ,m_dividend_sd[,"SD"])

  m_dividend <- data.table(m_dividend)
  setnames(m_dividend, c("M","SD"), c("dividend_mean","dividend_sd"))
  rm(m_dividend_mean, m_dividend_sd)

  ### join probabilistic and deterministic calculations into
  ### a single data table.

  data <- cbind(det, m_n_smokers)
  data <- cbind(data, m_mean_week_spend)
  data <- cbind(data, m_total_wk_exp)
  data <- cbind(data, m_total_wk_exp_up)
  data <- cbind(data, m_total_annual_exp)
  data <- cbind(data, m_total_annual_exp_up)
  data <- cbind(data, m_dividend)

  ### sort data

  data <- data[,c("UTLAcode", "UTLAname", "pop_n", "income", "smk_prev",
                  "n_smokers","n_smokers_mean","n_smokers_sd",
                  "mean_week_spend","mean_week_spend_mean","mean_week_spend_sd",
                  "total_wk_exp","total_wk_exp_mean","total_wk_exp_sd",
                  "total_wk_exp_up","total_wk_exp_up_mean","total_wk_exp_up_sd",
                  "total_annual_exp","total_annual_exp_mean","total_annual_exp_sd",
                  "total_annual_exp_up","total_annual_exp_up_mean","total_annual_exp_up_sd",
                  "dividend","dividend_mean","dividend_sd")]


  return(data)
}
