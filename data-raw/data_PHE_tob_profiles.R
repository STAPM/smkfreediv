library(data.table)

## https://fingertips.phe.org.uk/profile/tobacco-control/data#page/0/gid/1938132885/pat/6/ati/302/are/E06000037/iid/93798/age/168/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1

## Geography version : Counties and UAs (2019/20)
## Geography : Area type to group areas by - Region
## Topic: Smoking prevalence in adults

region_list <- c("southeast","southwest","london","eastofengland",
                 "eastmidlands","westmidlands","yorks",
                 "northeast","northwest")

for (r in region_list) {

profiles <- read.csv(paste0("data-raw/local tobacco profiles/",r," 2019.csv"))
setDT(profiles)

profiles <- profiles[Area.Code != "E92000001",]

profiles <- profiles[Indicator.Name == "Smoking Prevalence in adults (18+) - current smokers (APS)" &
                       Parent.Name != "",
                        c("Parent.Name","AreaName","Area.Code","Value",
                        "Upper.CI.95.0.limit","Lower.CI.95.0.limit","Count")]

setnames(profiles,
         names(profiles),
         c("gor","UTLAname","UTLAcode","smk_prev","smk_prev_uci","smk_prev_lci","n_smokers"))

###

if (r == "southeast") {
data_out <- copy(profiles)
} else {
data_out <- rbindlist(list(data_out,profiles))
}

rm(profiles)

}

profiles <- copy(data_out)

## get confidence interval on the number of smokers by backing out the population from
## the prevalence estimate and number of smokers, then applying the prevalence confidence
## intervals to the total population

## calculate population size, and smoking prevalence standard error

profiles[, pop_n := round(n_smokers/(smk_prev/100))]
profiles[, smk_prev_se := round((smk_prev_uci - smk_prev)/1.96,4)]

## simulate smoking prevalence to calculate standard error of the number of smokers

reps <- 1000

m_n_smokers     = matrix(rep(NA, 151*reps), ncol = reps)

for (i in 1:reps) {

  cat("\t\t1. Simulating Number of Smokers from the CIs", round(100*i/reps,2),"%", "               \r")
  utils::flush.console()
  if(i == reps) { cat("\n") }

  sim_data <- copy(profiles)

  sim_data[, prob_smk_prev := rnorm(1, smk_prev, smk_prev_se), by = "UTLAname"]
  sim_data[, prob_n_smokers := pop_n * (prob_smk_prev/100)]

  m_n_smokers[,i] <- as.vector(as.matrix(sim_data[,"prob_n_smokers"]))

}

m_n_smokers     <- data.table(m_n_smokers)
m_s <- transform(m_n_smokers, n_smokers_se=apply(m_n_smokers,1, sd, na.rm = TRUE))

m_n_smokers   <- m_s[,"n_smokers_se"]

############# Tidy data and save out

profiles <- cbind(profiles,m_n_smokers)

profiles <- profiles[,c("gor","UTLAcode","UTLAname","pop_n",
                        "smk_prev","smk_prev_se",
                        "n_smokers","n_smokers_se")]

PHE_tobacco_profiles <- copy(profiles)

PHE_tobacco_profiles[gor == "London region", gor := "London"]
PHE_tobacco_profiles[gor == "Yorkshire and the Humber region", gor := "Yorkshire and the Humber"]
PHE_tobacco_profiles[gor == "West Midlands region", gor := "West Midlands"]
PHE_tobacco_profiles[gor == "East Midlands region", gor := "East Midlands"]
PHE_tobacco_profiles[gor == "South East region", gor := "South East"]
PHE_tobacco_profiles[gor == "South West region", gor := "South West"]
PHE_tobacco_profiles[gor == "North East region", gor := "North East"]
PHE_tobacco_profiles[gor == "North West region", gor := "North West"]
PHE_tobacco_profiles[gor == "East of England region", gor := "East of England"]

PHE_tobacco_profiles <- PHE_tobacco_profiles[order(UTLAname),]


usethis::use_data(PHE_tobacco_profiles, overwrite = TRUE)
