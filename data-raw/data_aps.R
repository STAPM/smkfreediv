
## The purpose of this code is to use 2018 Annual Population Survey data to estimate the proportion
## of total UK smokers who are in England, to adjust total duty receipts by HMRC to an estimated
## England-only figure.


library(data.table)
library(apsclean)

root <- "D:/"
file <- "Datasets/Annual Population Survey/tab/"

data <- apsclean::combine_years(list(aps_read_2011(root, file),
                                     aps_read_2012(root, file),
                                     aps_read_2013(root, file),
                                     aps_read_2014(root, file),
                                     aps_read_2015(root, file),
                                     aps_read_2016(root, file),
                                     aps_read_2017(root, file),
                                     aps_read_2018(root, file)))

clean_data <- data %>%
  apsclean::aps_clean_demographic() %>%
  apsclean::aps_clean_smoking()

clean_data <- clean_data[,c("year","country","current_smoker","pwt")]

### generate weighted number of smokers by country/year

clean_data[, count := ifelse(current_smoker == "yes",1,0)]

collapse <- clean_data[, .(total = sum(count*pwt, na.rm = TRUE)), by = c("year","country")]

### reshape wide, create total smokers and proportion by country

wide <- dcast(collapse,
              year ~ country)

setnames(wide,
         names(wide),
         c("year","N_Eng","N_NI","N_Scot","N_Wales"))

wide[,N_uk := N_Eng + N_NI + N_Scot + N_Wales]

wide[, prop_Eng   := N_Eng / N_uk]
wide[, prop_NI    := N_NI / N_uk]
wide[, prop_Scot  := N_Scot / N_uk]
wide[, prop_Wales := N_Wales / N_uk]

prop_smokers_ENG <- mean(wide[,prop_Eng])

usethis::use_data(prop_smokers_ENG,overwrite=TRUE)

