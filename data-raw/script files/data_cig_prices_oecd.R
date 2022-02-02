### Read in OECD prices (weighted average prices) for 2016

library(data.table)
library(readxl)

price_cigs_oecd <- readxl::read_excel(path = "data-raw/tax-burden-cigarettes-ctt-trends.xlsx",
                                      sheet = "2016",
                                      range = "A3:I39")

setDT(price_cigs_oecd)

setnames(price_cigs_oecd,
         names(price_cigs_oecd),
         c("country","price_usd_excl_tax","duty_percent","avt_percent","vat_percent","tax_percent","currency","price","price_usd"))


usethis::use_data(price_cigs_oecd,overwrite = TRUE)
