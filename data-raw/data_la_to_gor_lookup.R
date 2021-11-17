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

###### UTLA to GOR lookup table

lookup <- read.csv("data-raw/utla_to_gor.csv")
setDT(lookup)

setnames(lookup, names(lookup), c("UTLAname","gor"))

lookup <- lookup[order(UTLAname),]
lookup[UTLAname == "Buckinghamshire", UTLAname := "Buckinghamshire UA"]


#n <- smkfreediv::localauthorities[substr(UTLAcode,1,1) == "E",]
#n <- unique(n[,c("UTLAcode","UTLAname")])

#merge <- merge(lookup, n, by = "UTLAname", all = T)

utla_gor_lookup <- copy(lookup)
usethis::use_data(utla_gor_lookup, overwrite = TRUE)
