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

codes <- as.matrix(as.vector(data_out[,"UTLAcode"]))
##### read in population numbers

pop_data <- readxl::read_excel("data-raw/ukmidyearestimates20192019ladcodes.xls",
                               sheet = paste0("MYE2 - Persons"),
                               range = "A5:CQ435")
setDT(pop_data)

pop_data <- pop_data[Code %in% codes ,]
pop_data <- pop_data[, c(1:2,23:95)]

total_pop <- rowSums(pop_data[,3:length(pop_data)])
pop_data <- data.table(pop_data,total_pop)
pop_data <- pop_data[, c("Code","Name","total_pop")]

setnames(pop_data, names(pop_data), c("UTLAcode","UTLAname","pop_n"))

pop_data[UTLAname == "Kingston upon Hull, City of", UTLAname := "Kingston upon Hull"]
pop_data[UTLAname == "Herefordshire, County of", UTLAname := "Herefordshire"]
pop_data[UTLAname == "Bristol, City of", UTLAname := "Bristol"]

pop_counts <- copy(pop_data)

usethis::use_data(pop_counts,overwrite = T)

