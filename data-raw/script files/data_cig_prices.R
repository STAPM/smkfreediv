library(data)

price_cigs <- read.csv("data-raw/cig_prices_ons.csv")
setDT(price_cigs)
price_cigs[,price := price/100]

usethis::use_data(price_cigs,overwrite = TRUE)
