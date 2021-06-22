#### local authority to GOR lookup table

library(readxl)
library(data.table)

inc <- readxl::read_excel("data-raw/incomeestimatesforsmallareasdatasetfinancialyearending20181.xls",
                          sheet = "Net annual income",
                          range = "C5:F7206")

inc <- unique(inc)

setDT(inc)

setnames(inc,
         names(inc),
         c("LAcode","LAname","GORcode","GORname"))

la_gor_lookup <- copy(inc)

usethis::use_data(la_gor_lookup, overwrite = TRUE)
