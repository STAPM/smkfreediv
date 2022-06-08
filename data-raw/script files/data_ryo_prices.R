
##### read in ONS price quotes data for the RPI/CPI in December 2018
##### https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes

library(data.table)
library(magrittr)
library(ggplot2)

data <- read.csv("data-raw/upload-pricequotes201812.csv") %>% setDT

ryo_product_ids <- c(320206)

data <- data[ITEM_ID == 320206]

### 149 RYO price observations (30 grams)

## scale up to 100 grams

data[, price_100g := (100/30)*PRICE]
data <- data[ price_100g != 0,]

ggplot(data) + aes(x = price_100g) + geom_density()

price <- data[, .(price = mean(price_100g))]

price_ryo_ons = round( as.numeric(price) , 2 )

usethis::use_data(price_ryo_ons, overwrite = TRUE)
