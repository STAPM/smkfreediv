#### construct population counts by local authority

library(readxl)
library(data.table)

  data <- readxl::read_excel("data-raw/ukmidyearestimates20192019ladcodes.xls",
                             sheet = paste0("MYE2 - Persons"),
                             range = "A5:CQ9")
  setDT(data)

  data <- data[Geography1 %in% c("Unitary Authority") ,]
  data[, Sex := w]

  data <- melt(data, id.vars = "Sex", value.name = "population", variable.name = "Age")

  assign(paste0("data_",w), data)




pop_counts <- rbindlist(list(data_Males,data_Females))


pop_counts <- pop_counts[, Sex := factor(Sex,
                                         levels = c("Females","Males"),
                                         labels = c("Female","Male"))]

pop_counts[, Age := as.numeric(Age) - 1]

pop_counts[ , Ageband := c("0-15" ,"16-24", "25-34", "35-44", "45-54", "55-64", "65+")[findInterval(Age, c(0, 16, 25, 35, 45, 55, 65, 1000))]]


