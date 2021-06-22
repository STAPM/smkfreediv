#' Construct UTLA Average Income
#'
#' Select one of the stored income variables to construct at upper-tier local authority level from the
#' variables stored in the package data \code{la_inc_pop} at lower-tier local authority level.
#'
#' @param mapping_data Data table. Mapping from lower-tier to upper-tier local authorities
#' @param income_data Data table
#' @param income_var Integer. Option governing which income variable is used. 1 for net household income,
#' 2 for net household income equivalised, and 3 for net household income equivalised after housing costs.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
GetIncome <- function(mapping_data = smkfreediv::localauthorities,
                      income_data = smkfreediv::la_inc_pop,
                      income_var = 3) {

  ### grab the income variable desired for the analysis

  if (income_var == 1) {
    income_vars <- data.table(income_data[,"net_annual_inc"],
                              income_data[,"net_annual_inc_M"],
                              income_data[,"net_annual_inc_SD"])

  } else if (income_var == 2) {
    income_vars <- data.table(income_data[,"net_annual_inc_eq"],
                              income_data[,"net_annual_inc_eq_M"],
                              income_data[,"net_annual_inc_eq_SD"])

  } else if (income_var == 3) {
    income_vars <- data.table(income_data[,"net_annual_inc_eq_disp"],
                              income_data[,"net_annual_inc_eq_disp_M"],
                              income_data[,"net_annual_inc_eq_disp_SD"])
  }

  income <- income_data[,c("LAcode","LAname","pop_2019")]

  income_final <- cbind(income, income_vars)

  setnames(income_final,
           names(income_final),
           c("LAcode","LAname","pop_2019",
             "income","income_sim_mean","income_sim_sd"))

  ### merge income and population data to the local authority mapping

  merge <- merge(income_final , mapping_data, by = c("LAcode","LAname"))

  rm(income_vars, income_final, income)
  ### generate mean income at UTLA level using population in 2019 as weights
  ### (deterministic calculation)

  data <- merge[, .(income = weighted.mean(income, w = pop_2019, na.rm = TRUE),
                    population = sum(pop_2019, na.rm = TRUE)),
                by = c("UTLAcode","UTLAname")]

  ### generate mean income at UTLA level using population in 2019 as weights
  ### (probabilistic calculation)

  data_pop <- merge[, income_sim := rnorm(1,income,income_sim_sd), by = c("LAcode","LAname")]

  data_pop <- merge[, .(income_sim = weighted.mean(income_sim, w = pop_2019, na.rm = TRUE),
                    population = sum(pop_2019, na.rm = TRUE)),
                by = c("UTLAcode","UTLAname")]

  ### merge probabilistic to deterministic

  data_out <- merge(data, data_pop, by = c("UTLAcode","UTLAname","population"))

  data_out <- data_out[order(UTLAname)]

  return(data_out)
}
