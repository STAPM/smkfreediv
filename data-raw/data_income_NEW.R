library(readxl)
library(data.table)

### read in income per head data, and population data to use as a weight

## income
income <- read_excel(path = "data-raw/vcregionalgdhibylareordered.xlsx",
                     sheet = "GDHI per head",
                     range = "A3:W351")

setDT(income)
income <- income[,c("Region","LAU1 code","LA name","2016")]

setnames(income,
         names(income),
         c("Region","LAcode","LAname","GDHI"))

## population

pop <- read_excel(path = "data-raw/vcregionalgdhibylareordered.xlsx",
                  sheet = "Population",
                  range = "A3:W351")

setDT(pop)
pop <- pop[,c("Region","LAU1 code","LA name","2016")]

setnames(pop,
         names(pop),
         c("Region","LAcode","LAname","pop"))

inc_pop_merge <- merge(income, pop, by = c("Region","LAcode","LAname"))

### read in the mapping data

mapping <- read.csv("data-raw/LTLA to UTLA England and Wales 2017 2019.csv")
setDT(mapping)

mapping <- mapping[,1:7]

setnames(mapping,
         names(mapping),
         c("LAcode","LAname","UTLAcode2017","UTLAname2017","FID","UTLAcode","UTLAname"))

## deal with slight mismatch in the names for E07000146

inc_pop_merge[LAcode == "E07000146", LAname := "King's Lynn and West Norfolk"]
mapping[LAcode == "E07000146", LAname := "King's Lynn and West Norfolk"]

data <- merge(inc_pop_merge,mapping, by = c("LAcode","LAname"), sort = F)


### create average disposable income at UTLA level

utla <- data[, .(income = weighted.mean(GDHI, w = pop)),
             by = c("UTLAname","UTLAcode")]

utla <- utla[order(UTLAname),]

utla[,country := ifelse(substring(UTLAcode,1,1) == "W","Wales","England")]

#### tidy names so they match with tobacco profiles

utla[UTLAcode == "E10000002", UTLAcode := "E06000060"]
utla[UTLAcode == "E10000009", UTLAcode := "E06000059"]

utla[UTLAname == "Kingston upon Hull, City of", UTLAname := "Kingston upon Hull"]
utla[UTLAname == "Herefordshire, County of", UTLAname := "Herefordshire"]
utla[UTLAname == "Bristol, City of", UTLAname := "Bristol"]
utla[UTLAname == "Buckinghamshire", UTLAname := "Buckinghamshire UA"]

utla_inc_data <- copy(utla)

usethis::use_data(utla_inc_data, overwrite = TRUE)

