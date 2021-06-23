library(data.table)

profiles <- read.csv("data-raw/PHE tobacco profiles 2019.csv")
setDT(profiles)

profiles <- profiles[Area.Code != "E92000001",]

profiles <- profiles[,c("AreaName","Area.Code","Value",
                        "Upper.CI.95.0.limit","Lower.CI.95.0.limit",
                        "Count")]

profiles[, Count := as.numeric(gsub(",","",Count))]

setnames(profiles,
         names(profiles),
         c("UTLAname","UTLAcode","smk_prev","smk_prev_uci","smk_prev_lci","n_smokers"))

## get confidence interval on the number of smokers by backing out the population from
## the prevalence estimate and number of smokers, then applying the prevalence confidence
## intervals to the total population

## calculate population size, and smoking prevalence standard error

profiles[, pop_n := round(n_smokers/(smk_prev/100))]
profiles[, smk_prev_se := round((smk_prev_uci - smk_prev)/1.96,4)]

## simulate smoking prevalence to calculate standard error of the number of smokers

reps <- 50000

m_n_smokers     = matrix(rep(NA, 151*reps), ncol = reps)

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

tobacco_profiles <- copy(profiles)

usethis::use_data(tobacco_profiles, overwrite = TRUE)
