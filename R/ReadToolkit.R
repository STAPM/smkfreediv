#' Read Toolkit Data
#'
#' Reads in the raw Smoking Toolkit Study (STS) data in SPSS format
#' and saves to a chosen directory in RDS format as well as returning it as
#' an R object
#'
#' @param path path to the directory where the raw SPSS data is saved
#' @param data name of the data file (defaults to the April 2021 version)
#' @param save Logical. If TRUE, saves out a copy of the data in RDS format to the same directory as the raw data.
#' @param name Character. Name of the file to export
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
ReadToolkit <- function(
  path,
  data ,
  save = TRUE,
  name
) {

  STS_data <- foreign::read.spss(paste0(path, data), to.data.frame = TRUE)

  setDT(STS_data)

  setnames(STS_data, stringr::str_replace(colnames(STS_data), "X.", "A"))

  if (save) {
  saveRDS(STS_data,paste0(path, name, ".rds"))
  }

  return(STS_data)
}
