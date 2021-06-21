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

profiles[, pop_n := round(n_smokers/(smk_prev/100))]

profiles[, n_smokers_uci := round(pop_n * (smk_prev_uci/100)) ]
profiles[, n_smokers_lci := round(pop_n * (smk_prev_lci/100)) ]

## get standard errors (normal distribution)

profiles[, n_smokers_se := round((n_smokers_uci - n_smokers)/1.96)]
profiles[, smk_prev_se := round((smk_prev_uci - smk_prev)/1.96,4)]

sum(profiles$n_smokers, na.rm = T)

profiles <- profiles[,c("UTLAcode","UTLAname","pop_n",
                        "smk_prev","smk_prev_uci","smk_prev_lci","smk_prev_se",
                        "n_smokers","n_smokers_uci","n_smokers_lci","n_smokers_se")]

tobacco_profiles <- profiles

usethis::use_data(tobacco_profiles, overwrite = TRUE)
