## The aim of this code is to read in the raw toolkit data from .sav format,
## clean, and save out a filtered/cleaned dataset in .rds format ready for analysis.

library(smkfreediv)
library(data.table)

memory.limit(size=16339) ## so work laptop can read the data

## (1) Read in the data - save an RDS data file to the data input folder so this
##                        part of the code need only be run once.

data_file <- "omni174_39.1_65.2cot_31.3a_25.4s_recodes_60.5sa.sav" # data

toolkit_raw <- smkfreediv::ReadToolkit(path = "data-raw/",
                                       data = data_file,
                                       save = FALSE)

## (2) Clean the data, retain only needed variables and waves, and save the
##     cleaned data back to the raw inputs folder

## Use period April 2014 - February 2020


toolkit_clean <- smkfreediv::CleanToolkit(data = toolkit_raw,
                                          start_month = 90,
                                          end_month = 160)

## (3) Deflate expenditure to December 2018

toolkit_clean <- smkfreediv::DeflateToolkit(data = toolkit_clean,
                                            index = smkfreediv::cpi_tobacco,
                                            base_month = 12,
                                            base_year = 2018)

TOOLKIT <- copy(toolkit_clean)

usethis::use_data(TOOLKIT, overwrite = TRUE)

