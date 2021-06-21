library(readxl)
library(data.table)
library(stringr)

reps <- 200000

#################################################################################
######### Get LTLA level income data and population data to use to weight up
######### the income data to UTLA level

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

### use confidence intervals to generate probabilistic mean/se income for each LA

dim = dim(income_la)[1]*reps

mean     = matrix(rep(NA, dim), ncol = reps)

set.seed(2021)
for (i in 1:reps) {

  # Simulating
  cat("\t\tNet Income Replication", i, "of", reps, "               \r")
  utils::flush.console()
  if(i == reps) { cat("\n") }

  # copy of the original MSOA level data
  income_copy <- copy(income)

  # construct the standard error from the 95% confidence interval
  income_copy[, se := (income - lci)/1.96]

  # draw a variable which has a mean equal to the value, sd equal to standard error
  income_copy[, prob := rnorm(1,mean = income, sd = se) ,
               by = c("LAcode","LAname")]

  income_copy <- income_copy[, .(income = mean(prob)), by = c("LAcode","LAname")]

  income_copy <- income_copy[order(LAname),]

  mean[, i]    <- as.matrix(as.vector(income_copy[, "income"]))

  rm(income_copy)
}

mean_data <- data.table(mean)

m   <- transform(mean_data, SD=apply(mean_data,1, mean, na.rm = TRUE))
setnames(m, "SD", "mean")
sd  <- transform(mean_data, SD=apply(mean_data,1, sd, na.rm = TRUE))
setnames(sd, "SD", "sd")
net_annual_inc      <- cbind(m[,"mean"], sd[,"sd"])

setnames(net_annual_inc, c("mean","sd"), c("net_annual_inc_mean","net_annual_inc_sd"))

# copy in deterministic mean

net_annual_inc <- cbind(income_la, net_annual_inc)
setnames(net_annual_inc, "income", "net_annual_inc")

rm(m, sd, mean, mean_data, income_la, income)

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

### use confidence intervals to generate probabilistic mean/se income for each LA

dim = dim(income_la)[1]*reps

mean     = matrix(rep(NA, dim), ncol = reps)

set.seed(2021)
for (i in 1:reps) {

  # Simulating
  cat("\t\tNet Eq Income Replication", i, "of", reps, "               \r")
  utils::flush.console()
  if(i == reps) { cat("\n") }

  # copy of the original MSOA level data
  income_copy <- copy(income)

  # construct the standard error from the 95% confidence interval
  income_copy[, se := (income - lci)/1.96]

  # draw a variable which has a mean equal to the value, sd equal to standard error
  income_copy[, prob := rnorm(1,mean = income, sd = se) ,
              by = c("LAcode","LAname")]

  income_copy <- income_copy[, .(income = mean(prob)), by = c("LAcode","LAname")]

  income_copy <- income_copy[order(LAname),]

  mean[, i]    <- as.matrix(as.vector(income_copy[, "income"]))

  rm(income_copy)
}

mean_data <- data.table(mean)

m   <- transform(mean_data, SD=apply(mean_data,1, mean, na.rm = TRUE))
setnames(m, "SD", "mean")
sd  <- transform(mean_data, SD=apply(mean_data,1, sd, na.rm = TRUE))
setnames(sd, "SD", "sd")
net_annual_inc_eq      <- cbind(m[,"mean"], sd[,"sd"])

setnames(net_annual_inc_eq, c("mean","sd"), c("net_annual_inc_eq_mean","net_annual_inc_eq_sd"))

# copy in deterministic mean

net_annual_inc_eq <- cbind(income_la, net_annual_inc_eq)
setnames(net_annual_inc_eq, "income", "net_annual_inc_eq")

rm(m, sd, mean, mean_data, income_la, income)


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

### use confidence intervals to generate probabilistic mean/se income for each LA

dim = dim(income_la)[1]*reps

mean     = matrix(rep(NA, dim), ncol = reps)

set.seed(2021)
for (i in 1:reps) {

  # Simulating
  cat("\t\tNet Eq Income After Housing Replication", i, "of", reps, "               \r")
  utils::flush.console()
  if(i == reps) { cat("\n") }

  # copy of the original MSOA level data
  income_copy <- copy(income)

  # construct the standard error from the 95% confidence interval
  income_copy[, se := (income - lci)/1.96]

  # draw a variable which has a mean equal to the value, sd equal to standard error
  income_copy[, prob := rnorm(1,mean = income, sd = se) ,
              by = c("LAcode","LAname")]

  income_copy <- income_copy[, .(income = mean(prob)), by = c("LAcode","LAname")]

  income_copy <- income_copy[order(LAname),]

  mean[, i]    <- as.matrix(as.vector(income_copy[, "income"]))

  rm(income_copy)
}

mean_data <- data.table(mean)

m   <- transform(mean_data, SD=apply(mean_data,1, mean, na.rm = TRUE))
setnames(m, "SD", "mean")
sd  <- transform(mean_data, SD=apply(mean_data,1, sd, na.rm = TRUE))
setnames(sd, "SD", "sd")
net_annual_inc_eq_disp      <- cbind(m[,"mean"], sd[,"sd"])

setnames(net_annual_inc_eq_disp, c("mean","sd"), c("net_annual_inc_eq_disp_mean","net_annual_inc_eq_disp_sd"))

# copy in deterministic mean

net_annual_inc_eq_disp <- cbind(income_la, net_annual_inc_eq_disp)
setnames(net_annual_inc_eq_disp, "income", "net_annual_inc_eq_disp")

rm(m, sd, mean, mean_data, income_la, income)


##################################
#### merge in population data ####

merge1 <- merge(pop, net_annual_inc, by = c("LAcode","LAname"))
merge2 <- merge(merge1, net_annual_inc_eq, by = c("LAcode","LAname"))
merge3 <- merge(merge2, net_annual_inc_eq_disp, by = c("LAcode","LAname"))

la_inc_pop <- copy(merge3)

rm(merge1, merge2, merge3, pop, net_annual_inc, net_annual_inc_eq, net_annual_inc_eq_disp)
#### save out final data


usethis::use_data(la_inc_pop, overwrite = TRUE)
