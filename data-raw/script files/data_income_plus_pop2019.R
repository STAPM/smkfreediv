library(readxl)
library(data.table)
library(stringr)

reps <- 1000
set.seed(2021)

#################################################################################
######### Get LTLA level income data and population data to use to weight up
######### the income data to UTLA level in the dividend calculations

### Read population data

pop <- readxl::read_excel("data-raw/nomis_2021_06_17_155748_population_16plus_Apr15LAs.xlsx",
                          sheet = "Data",
                          range = "A7:B435")
setDT(pop)
setnames(pop,names(pop),c("LAname","pop_2019"))

### get list of local authorities in the income data

inc <- readxl::read_excel("data-raw/incomeestimatesforsmallareasdatasetfinancialyearending20181.xls",
                          sheet = "Net annual income",
                          range = "C5:D7206")
setDT(inc)
setnames(inc,c("Local authority name","Local authority code"),c("LAname","LAcode"))

inc <- inc[substring(LAcode,1,1) == "E"]

inc[, LAcode := stringr::str_trim(LAcode)]

inc <- unique(inc)
inc_names <- as.vector(as.matrix(inc[,"LAname"]))

### keep population local authorities that are in the income data

pop <- pop[LAname %in% inc_names, ]

pop <- merge(inc, pop, by = "LAname", all = TRUE)
pop <- pop[order(LAname)]

rm(inc, inc_names)







###################################
#### Read in income data proper

# for each local authority produce a simulated mean/standard deviation income measure.
# - use the mean and confidence intervals at MSOA level to do a random draw for each MSOA
# - construct the mean income
# - repeat the draws a large number of times
# - construct, for each local authority, the simulated mean and standard deviation


##############################
### Net household income #####

income <- readxl::read_excel("data-raw/incomeestimatesforsmallareasdatasetfinancialyearending20181.xls",
                             sheet = "Net annual income",
                             range = "A5:I7206")
setDT(income)

setnames(income,
         names(income),
         c("MSOAcode","MSOAname","LAcode","LAname","GORcode","GOR","income","uci","lci"))

income <- income[substring(LAcode,1,1) == "E"]
income <- income[order(LAname),]

### deterministic calculation of mean income
income_la <- income[, .(income = mean(income)), by = c("LAcode","LAname")]
income_la <- income_la[order(LAname),]

### probabilistic calculation of mean income

# generate standard deviation from the CI, then draw a random normal
# variable from the point estimate of income and the calculated SD

# repeat this multiple times and store in a matrix

m_income     = matrix(rep(NA, 326*reps), ncol = reps)

for (i in 1:reps) {

  cat("\t\t1. Simulating Net Income", round(100*i/reps,2),"%", "               \r")
  utils::flush.console()
  if(i == reps) { cat("\n") }

data_prob <- income[, income_sim_sd := (income - lci)/1.96]
data_prob <- data_prob[, income_sim := rnorm(1,income,income_sim_sd),
                    by = c("MSOAcode","MSOAname",
                           "LAcode","LAname")]

data_prob <- data_prob[, .(income_sim = mean(income_sim, na.rm = TRUE)),
                  by = c("LAcode","LAname")]

m_income[,i] <- as.vector(as.matrix(data_prob[,"income_sim"]))
}
## make result matrix into a data table and take mean and SD of each row
m_income     <- data.table(m_income)

m_income_m <- transform(m_income, M=apply(m_income,1, mean, na.rm = TRUE))
m_income_s <- transform(m_income, SD=apply(m_income,1, sd, na.rm = TRUE))
m_income   <- cbind(m_income_m[,"M"] ,m_income_s[,"SD"])

# copy in deterministic mean

net_annual_inc <- cbind(income_la, m_income)
setnames(net_annual_inc,
         c("income","M","SD"),
         c("net_annual_inc","net_annual_inc_M","net_annual_inc_SD"))

rm(income, m_income, m_income_m, m_income_s, income_la, data_prob)







#################################################################
### Net household income (equivalised) before housing costs #####

income <- readxl::read_excel("data-raw/incomeestimatesforsmallareasdatasetfinancialyearending20181.xls",
                             sheet = "Net income before housing costs",
                             range = "A5:I7206")
setDT(income)

setnames(income,
         names(income),
         c("MSOAcode","MSOAname","LAcode","LAname","GORcode","GOR","income","uci","lci"))

income <- income[substring(LAcode,1,1) == "E"]
income <- income[order(LAname),]

### deterministic calculation of mean income
income_la <- income[, .(income = mean(income)), by = c("LAcode","LAname")]
income_la <- income_la[order(LAname),]

### probabilistic calculation of mean income

# generate standard deviation from the CI, then draw a random normal
# variable from the point estimate of income and the calculated SD

# repeat this multiple times and store in a matrix

m_income     = matrix(rep(NA, 326*reps), ncol = reps)

for (i in 1:reps) {

cat("\t\t2. Simulating Net Income", round(100*i/reps,2),"%", "               \r")
utils::flush.console()
if(i == reps) { cat("\n") }

  data_prob <- income[, income_sim_sd := (income - lci)/1.96]
  data_prob <- data_prob[, income_sim := rnorm(1,income,income_sim_sd),
                         by = c("MSOAcode","MSOAname",
                                "LAcode","LAname")]

  data_prob <- data_prob[, .(income_sim = mean(income_sim, na.rm = TRUE)),
                         by = c("LAcode","LAname")]

  m_income[,i] <- as.vector(as.matrix(data_prob[,"income_sim"]))
}
## make result matrix into a data table and take mean and SD of each row
m_income     <- data.table(m_income)

m_income_m <- transform(m_income, M=apply(m_income,1, mean, na.rm = TRUE))
m_income_s <- transform(m_income, SD=apply(m_income,1, sd, na.rm = TRUE))
m_income   <- cbind(m_income_m[,"M"] ,m_income_s[,"SD"])

# copy in deterministic mean

net_annual_inc_eq <- cbind(income_la, m_income)
setnames(net_annual_inc_eq,
         c("income","M","SD"),
         c("net_annual_inc_eq","net_annual_inc_eq_M","net_annual_inc_eq_SD"))

rm(income, m_income, m_income_m, m_income_s, income_la, data_prob)








################################################################
### Net household income (equivalised) after housing costs #####

income <- readxl::read_excel("data-raw/incomeestimatesforsmallareasdatasetfinancialyearending20181.xls",
                             sheet = "Net income after housing costs",
                             range = "A5:I7206")
setDT(income)

setnames(income,
         names(income),
         c("MSOAcode","MSOAname","LAcode","LAname","GORcode","GOR","income","uci","lci"))

income <- income[substring(LAcode,1,1) == "E"]
income <- income[order(LAname),]

### deterministic calculation of mean income
income_la <- income[, .(income = mean(income)), by = c("LAcode","LAname")]
income_la <- income_la[order(LAname),]

### probabilistic calculation of mean income

# generate standard deviation from the CI, then draw a random normal
# variable from the point estimate of income and the calculated SD

# repeat this multiple times and store in a matrix

m_income     = matrix(rep(NA, 326*reps), ncol = reps)

for (i in 1:reps) {

  cat("\t\t3. Simulating Net Income", round(100*i/reps,2),"%", "               \r")
  utils::flush.console()
  if(i == reps) { cat("\n") }

  data_prob <- income[, income_sim_sd := (income - lci)/1.96]
  data_prob <- data_prob[, income_sim := rnorm(1,income,income_sim_sd),
                         by = c("MSOAcode","MSOAname",
                                "LAcode","LAname")]

  data_prob <- data_prob[, .(income_sim = mean(income_sim, na.rm = TRUE)),
                         by = c("LAcode","LAname")]

  m_income[,i] <- as.vector(as.matrix(data_prob[,"income_sim"]))
}
## make result matrix into a data table and take mean and SD of each row
m_income     <- data.table(m_income)

m_income_m <- transform(m_income, M=apply(m_income,1, mean, na.rm = TRUE))
m_income_s <- transform(m_income, SD=apply(m_income,1, sd, na.rm = TRUE))
m_income   <- cbind(m_income_m[,"M"] ,m_income_s[,"SD"])

# copy in deterministic mean

net_annual_inc_eq_disp <- cbind(income_la, m_income)
setnames(net_annual_inc_eq_disp,
         c("income","M","SD"),
         c("net_annual_inc_eq_disp","net_annual_inc_eq_disp_M","net_annual_inc_eq_disp_SD"))

rm(income, m_income, m_income_m, m_income_s, income_la, data_prob)


##################################
#### merge in population data ####

merge1 <- merge(pop, net_annual_inc, by = c("LAcode","LAname"))
merge2 <- merge(merge1, net_annual_inc_eq, by = c("LAcode","LAname"))
merge3 <- merge(merge2, net_annual_inc_eq_disp, by = c("LAcode","LAname"))

la_inc_pop <- copy(merge3)

rm(merge1, merge2, merge3, pop, net_annual_inc, net_annual_inc_eq, net_annual_inc_eq_disp)

###############################################################
### Adjust some LA codes and merge in UTLA names and codes ####

## match LA codes for Welwyn Hatfield and St Albans to those in the LA/UTLA lookup.

la_inc_pop[LAname == "St Albans", LAcode := "E07000240"]
la_inc_pop[LAname == "Welwyn Hatfield", LAcode := "E07000241"]

#### save out final data

la_inc_pop <- la_inc_pop[order(LAname),]

#usethis::use_data(la_inc_pop, overwrite = TRUE)


###########################################
## Merge in upper tier local authorities ##

localauthorities <- read.csv("data-raw/LTLA to UTLA England and Wales 2012 2019.csv")
setDT(localauthorities)

setnames(localauthorities,
         names(localauthorities),
         c("LAcode","LAname","UTLAcode","UTLAname"))

localauthorities <- localauthorities[substring(LAcode,1,1) == "E",]

income <- merge(localauthorities, la_inc_pop, by = c("LAcode","LAname"))

income <- income[order(UTLAname),]

### tidy up so the names and codes match exactly to the tobacco profiles

#income[UTLAname == "Buckinghamshire", UTLAcode := "E06000060"]
#income[UTLAname == "Buckinghamshire", UTLAname := "Buckinghamshire UA"]

# fix mis-match of Bristol naming
income[UTLAcode == "E06000023", UTLAname := "Bristol"]

# fix mis-match of Kingston upon Hull naming
income[UTLAcode == "E06000010", UTLAname := "Kingston upon Hull"]

# fix mis-match of Herefordshire naming
income[UTLAcode == "E06000019", UTLAname := "Herefordshire"]

# Dorset codes
income[UTLAcode == "E10000009", UTLAcode := "E06000059"]


usethis::use_data(income, overwrite = TRUE)
