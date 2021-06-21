library(data.table)

utlanames <- read.csv("data-raw/LTLA to UTLA England and Wales April 2019.csv")
setDT(utlanames)
utlanames <- utlanames[substring(LTLA19CD,1,1) == "E"]

setnames(utlanames,
         c("LTLA19CD","LTLA19NM","UTLA19CD","UTLA19NM"),
         c("LAcode","LAname","UTLAcode","UTLAname"))

localauthorities <- utlanames[, ï..FID := NULL]

usethis::use_data(localauthorities, overwrite = TRUE)
