#' Weekly Tobacco Spend
#'
#' Generates weekly spending on tobacco as a weighted mean across individuals in the sample.
#'
#' @param data Data table. The cleaned toolkit data.
#' @param strat_vars Character vector. Variables over which to calculate the weekly mean expenditure
#'
#' @importFrom data.table := copy rbindlist setDT
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
CalcWeekSpend <- function(data,
                      strat_vars) {

  ### only keep observations which are not missing for strat_vars

  for(cv in strat_vars) {

    if(cv %in% colnames(data)) {
      data <- data[!is.na(get(cv))]
    } else {
      warning(cv, " not a column in the data")
    }

  }
    ### calculate the mean

  data[, sample := ifelse(is.na(weekspend), 0, 1)]

  t <- data[, .(mean_week_spend = weighted.mean(weekspend, w = Aweight0 ,na.rm = T),
                se_week_spend = sd(weekspend, na.rm = T)/sqrt(.N),
                sample_tkit = sum(sample)
                ),
            by = strat_vars]

  ## drop non-English local authorities
  data_out <- t[substring(UTLAcode,1,1) == "E",]

  ### tidy up so the names and codes match exactly to the tobacco profiles

  # name of Buckinghamshire
  data_out[UTLAname == "Buckinghamshire", UTLAname := "Buckinghamshire UA"]

  # fix mis-match of Bristol naming
  data_out[UTLAcode == "E06000023", UTLAname := "Bristol"]

  # fix mis-match of Kingston upon Hull naming
  data_out[UTLAcode == "E06000010", UTLAname := "Kingston upon Hull"]

  # fix mis-match of Herefordshire naming
  data_out[UTLAcode == "E06000019", UTLAname := "Herefordshire"]

  return(data_out)
}