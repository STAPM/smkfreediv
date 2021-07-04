library(data.table)
library(readxl)

profiles <- read_excel("data-raw/PHW tobacco profiles 2020.xlsx",range = "C1:K23")
setDT(profiles)


## get confidence interval on the number of smokers by backing out the population from
## the prevalence estimate and number of smokers, then applying the prevalence confidence
## intervals to the total population

## calculate population size, and smoking prevalence standard error

profiles[, n_smokers := round(pop_n * (Rate/100))]
profiles[, smk_prev_se := round((UpperCI - Rate)/1.96,4)]

setnames(profiles, c("Area","Code","Rate"), c("UTLAname","UTLAcode","smk_prev"))

## simulate smoking prevalence to calculate standard error of the number of smokers

reps <- 50000

m_n_smokers     = matrix(rep(NA, 22*reps), ncol = reps)

for (i in 1:reps) {

  cat("\t\t1. Simulating Number of Smokers", round(100*i/reps,2),"%", "               \r")
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

profiles <- profiles[,c("UTLAcode","UTLAname","pop_n",
                        "smk_prev","smk_prev_se",
                        "n_smokers","n_smokers_se")]

PHW_tobacco_profiles <- copy(profiles)

usethis::use_data(PHW_tobacco_profiles, overwrite = TRUE)
