library(readxl)
library(data.table)



Year <- c(rep(1988:2021, each = 12) , rep(2022,5))
month <- c(rep(1:12, times = 2021 - 1988 + 1), 1:5)

data <- data.table(Year,month)

#data <- data[-c(403:408),]




cpi <- readxl::read_excel("data-raw/series-110722-tobacco-cpi.xls",
                           sheet = "data",
                           range = "A180:B592",
                           col_names = FALSE)

setDT(cpi)
setnames(cpi, names(cpi), c("t","cpi"))

cpi_tobacco <- cbind(data, cpi)

cpi_tobacco[, Month := factor(month,
                             levels = 1:12,
                             labels = c("January","February","March","April","May","June",
                                        "July","August","September","October","November","December"))]

cpi_tobacco <- cpi_tobacco[, c("Year","month","Month","cpi")]

### re-base to december 2018

cpi_base <- as.numeric(cpi_tobacco[Year == 2018 & month == 12, "cpi"])

cpi_tobacco[, cpi := 100*cpi/cpi_base]

usethis::use_data(cpi_tobacco, overwrite = TRUE)
