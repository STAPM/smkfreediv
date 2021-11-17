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

  mean_calcs <- copy(data)

  ### missing expenditure as 0

  mean_calcs[is.na(weekspend) & Smoker == 1, weekspend := 0]

  ### apply upshift factor to mean weekly spending
  mean_calcs[, weekspend := weekspend * upshift]

  ### only keep observations which are not missing for strat_vars

  for(cv in strat_vars) {

    if(cv %in% colnames(mean_calcs)) {
      mean_calcs <- mean_calcs[!is.na(get(cv))]
    } else {
      warning(cv, " not a column in the data")
    }

  }
    ### calculate the mean

  mean_calcs[, sample := ifelse(is.na(weekspend), 0, 1)]

  data_out <- mean_calcs[, .(mean_week_spend = weighted.mean(weekspend, w = Aweight0 ,na.rm = T),
                             se_week_spend = sqrt(spatstat::weighted.var(weekspend, na.rm = T)) / sqrt(.N),
                             median_week_spend = spatstat::weighted.median(weekspend, w = Aweight0, na.rm = T),
                             sample_tkit = sum(sample)
                ),
            by = strat_vars]

  return(data_out)
}
