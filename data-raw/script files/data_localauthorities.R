library(data.table)

localauthorities <- read.csv("data-raw/LTLA to UTLA England and Wales 2012 2019.csv")
setDT(localauthorities)

setnames(localauthorities,
         names(localauthorities),
         c("LAcode","LAname","UTLAcode","UTLAname"))

### tidy up so the names and codes match exactly to the tobacco profiles

income[UTLAname == "Buckinghamshire", UTLAcode := "E06000060"]
income[UTLAname == "Buckinghamshire", UTLAname := "Buckinghamshire UA"]

# fix mis-match of Bristol naming
income[UTLAcode == "E06000023", UTLAname := "Bristol"]

# fix mis-match of Kingston upon Hull naming
income[UTLAcode == "E06000010", UTLAname := "Kingston upon Hull"]

# fix mis-match of Herefordshire naming
income[UTLAcode == "E06000019", UTLAname := "Herefordshire"]

# Dorset codes
income[UTLAcode == "E10000009", UTLAcode := "E06000059"]

usethis::use_data(localauthorities, overwrite = TRUE)
