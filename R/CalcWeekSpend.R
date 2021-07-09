#' Weekly Tobacco Spend
#'
#' Generates weekly spending on tobacco as a weighted mean across individuals in the sample.
#'
#' @param data Data table. The cleaned toolkit data.
#' @param strat_vars Character vector. Variables over which to calculate the weekly mean expenditure
#' @param upshift Numeric. Parameter to upshift expenditures calculated from the toolkit data.
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
                          strat_vars,
                          upshift = 1) {

  ### apply upshift factor to mean weekly spending
  data[, weekspend := weekspend*upshift]

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

  data_out <- data[, .(mean_week_spend = weighted.mean(weekspend, w = Aweight0 ,na.rm = T),
                se_week_spend = sd(weekspend, na.rm = T)/sqrt(.N),
                sample_tkit = sum(sample)
                ),
            by = strat_vars]

  return(data_out)
}
