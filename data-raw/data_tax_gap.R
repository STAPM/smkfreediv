library(readxl)
library(data.table)

### read in tax gap tables to obtain figures for the share of illicit tobacco
### in total consumption, by product

### FM Cigs

data <- readxl::read_excel("data-raw/HMRC-tobacco-tax-gap-tables-2017.xlsx",
                           sheet = "Table 1.4",
                           range = "A3:R12")
setDT(data)

data <- data[c(3,7,9),]

setnames(data,c("...1"),c("Source"))

data[1, Source := "Legal"]
data[2, Source := "Illicit"]
data[3, Source := "Cross-Border"]

data <- melt(data,
             id.vars = "Source",
             variable.name = "year",
             value.name = "share_fm")

fm <- copy(data)

### RYO Tob

data <- readxl::read_excel("data-raw/HMRC-tobacco-tax-gap-tables-2017.xlsx",
                           sheet = "Table 1.6",
                           range = "A3:R15")
setDT(data)

data <- data[c(3,7,11),]

setnames(data,c("...1"),c("Source"))

data[1, Source := "Legal"]
data[2, Source := "Illicit"]
data[3, Source := "Cross-Border"]

data <- melt(data,
             id.vars = "Source",
             variable.name = "year",
             value.name = "share_ryo")

ryo <- copy(data)

##### Merge and retain most recent year

data <- merge(fm, ryo, by = c("Source","year"), sort = F)

data <- data[year == "2016-17", ]
data[, year := NULL ]

tax_gap_data <- copy(data)
usethis::use_data(tax_gap_data, overwrite = TRUE)
