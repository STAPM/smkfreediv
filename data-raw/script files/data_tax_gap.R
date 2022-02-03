library(readxl)
library(data.table)

### read in tax gap tables to obtain figures for the share of illicit tobacco
### in total consumption, by product


#########################
#### FM Cigarettes ######

### illicit market share

data <- readxl::read_excel("data-raw/Measuring_tax_gaps_tables_2021_edition.xlsx",
                           sheet = "Table 3.10 - 3.13",
                           range = "A3:U6")
setDT(data)

data <- data[c(2),]

setnames(data,c("Illicit market share"),c("variable"))

data[1, variable := "illicit_share"]

data <- melt(data,
             id.vars = "variable",
             variable.name = "year",
             value.name = "illicit_share_fm")


### illicit market volume (billion cigarettes)

data_vol <- readxl::read_excel("data-raw/Measuring_tax_gaps_tables_2021_edition.xlsx",
                           sheet = "Table 3.10 - 3.13",
                           range = "A32:U35")
setDT(data_vol)

data_vol <- data_vol[c(2),]

setnames(data_vol,c("Illicit market"),c("variable"))

data_vol[1, variable := "illicit_vol"]

data_vol <- melt(data_vol,
             id.vars = "variable",
             variable.name = "year",
             value.name = "illicit_volume_fm")

#### combine data tables

fm_data <- merge(data[,c("year","illicit_share_fm")],
                 data_vol[,c("year","illicit_volume_fm")],
                 by = "year", sort = F)

yr <- seq(2000:2019)

fm_data[, tax_year := year ]
fm_data[, year := yr + 2000 - 1]

#######################
#### RYO Tobacco ######

### illicit market share

data <- readxl::read_excel("data-raw/Measuring_tax_gaps_tables_2021_edition.xlsx",
                           sheet = "Table 3.14 - 3.17",
                           range = "A3:U6")
setDT(data)

data <- data[c(2),]

setnames(data,c("Illicit market share"),c("variable"))

data[1, variable := "illicit_share"]

data <- melt(data,
             id.vars = "variable",
             variable.name = "year",
             value.name = "illicit_share_ryo")


### illicit market volume (billion cigarettes)

data_vol <- readxl::read_excel("data-raw/Measuring_tax_gaps_tables_2021_edition.xlsx",
                               sheet = "Table 3.14 - 3.17",
                               range = "A32:U35")
setDT(data_vol)

data_vol <- data_vol[c(2),]

setnames(data_vol,c("Illicit market"),c("variable"))

data_vol[1, variable := "illicit_vol"]

data_vol <- melt(data_vol,
                 id.vars = "variable",
                 variable.name = "year",
                 value.name = "illicit_volume_ryo")

#### combine data tables

ryo_data <- merge(data[,c("year","illicit_share_ryo")],
                 data_vol[,c("year","illicit_volume_ryo")],
                 by = "year", sort = F)

yr <- seq(2000:2019)

ryo_data[, tax_year := year ]
ryo_data[, year := yr + 2000 - 1]


#########################################
### Merge RYO to FM data and save out ###

tax_gap_data <- merge(fm_data, ryo_data, by = c("year","tax_year"))


usethis::use_data(tax_gap_data, overwrite = TRUE)









