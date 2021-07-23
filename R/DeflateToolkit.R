#' Deflate Toolkit Data
#'
#' Use the ONS time series on cigarette prices to deflate tobacco expenditures
#' in the toolkit data to a base month and year
#'
#' @param data Data table. Toolkit data cleaned using the \code{CleanToolkit} function.
#' @param index Data table. Tobacco price data to use in the deflation. Default is the ONS tobacco CPI series.
#' @param base_month Numeric. Month of the year to deflate prices to.
#' @param base_year End month for the analysis period
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
DeflateToolkit <- function(data = data,
                           index = smkfreediv::cpi_tobacco,
                           base_month,
                           base_year) {

  ## rebase to chosen base month and year


  cpi_base <- as.numeric(cpi_tobacco[Year == base_year & month == base_month, "cpi"])
  index[, cpi := 100*cpi/cpi_base]

  ## merge to the toolkit data

  data <- merge(data, index, by = c("Year","Month"), all.x = TRUE, sort = FALSE)

  ## deflate

  data[, weekspend := weekspend*(100/cpi)]

  ## drop cpi

  data[, cpi := NULL]

  return(data)
}
