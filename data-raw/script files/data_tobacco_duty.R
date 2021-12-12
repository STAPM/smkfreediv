library(readxl)
library(data.table)

data <- read_excel("data-raw/2021_JUL_Tobacco_Tables.xlsx",
                   sheet = "Table_1_receipts",
                   range = "A5:F34",
                   col_names = FALSE)

setDT(data)

setnames(data,
         names(data),
         c("fin_year","FM_cigs","Cigars","RYO_tob","Other","Total"))


years <- read_excel("data-raw/2021_JUL_Tobacco_Tables.xlsx",
                    sheet = "Table_1_receipts",
                    range = "A36:A65",
                    col_names = FALSE)

setDT(years)

data <- cbind(years,data)

setnames(data,
         names(data),
         c("year","fin_year","FM_cigs","Cigars","RYO_tob","Other","Total"))


tobacco_duty_receipts <- data[,c("year","FM_cigs","RYO_tob","Cigars","Other","Total")]

usethis::use_data(tobacco_duty_receipts,overwrite = TRUE)
