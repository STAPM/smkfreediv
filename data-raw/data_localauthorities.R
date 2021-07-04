library(data.table)

localauthorities <- read.csv("data-raw/LTLA to UTLA England and Wales 2012 2019.csv")
setDT(localauthorities)

setnames(localauthorities,
         names(localauthorities),
         c("LAcode","LAname","UTLAcode","UTLAname"))

usethis::use_data(localauthorities, overwrite = TRUE)
