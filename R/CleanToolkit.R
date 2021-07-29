#' Clean Toolkit Data
#'
#' Clean the raw toolkit data to produce a dataset that can be used for the
#' smoke free dividend calculations
#'
#' @param data Data table. Toolkit data.
#' @param start_month Numeric. Toolkit tart month for the analysis period.
#' @param end_month Numeric. Toolkit end month for the analysis period.
#'
#' @importFrom data.table := copy rbindlist setDT
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
CleanToolkit <- function(data = data,
                         start_month,
                         end_month) {

  ### retain only the needed variables

  data <- data[, .(xwave,    # survey wave
                   month,    # survey month
                   Sex = sexz,     # sex
                   Aweight0,       # weight
                   Age = actage,   # age
                   qual,           # highest qualification
                   tenure,        # housing tenure
                   gor = gore, # government office region
                   grade,      # social grade
                   work,       # working status
                   ethnic,     # ethnicity
                   LAcode,     # local authority code
                   daily,      # current daily smoker
                   Smoker = smoker,   # smoking status
                   ExSmoker = exsmoker,

                   ### expenditures
                   weekspend

                   ### consumption/dependence
                   # q632x1, q632a9_1, q632a0_1, q632b1_1
  )]

  #######################
  #### TIME VARIABLES

  data[month %in% c(   3,  15, 27, 39, 51, 63, 75, 87, 99,  111, 123, 135, 147, 159, 171 ), Month :=  "January"]
  data[month %in% c(   4,  16, 28, 40, 52, 64, 76, 88, 100, 112, 124, 136, 148, 160, 172 ), Month :=  "February"]
  data[month %in% c(   5,  17, 29, 41, 53, 65, 77, 89, 101, 113, 125, 137, 149,      173 ), Month :=  "March"]
  data[month %in% c(   6,  18, 30, 42, 54, 66, 78, 90, 102, 114, 126, 138, 150, 162, 174), Month :=  "April"]
  data[month %in% c(   7,  19, 31, 43, 55, 67, 79, 91, 103, 115, 127, 139, 151, 163 ), Month :=  "May"]
  data[month %in% c(   8,  20, 32, 44, 56, 68, 80, 92, 104, 116, 128, 140, 152, 164 ), Month :=  "June"]
  data[month %in% c(   9,  21, 33, 45, 57, 69, 81, 93, 105, 117, 129, 141, 153, 165 ), Month :=  "July"]
  data[month %in% c(   10, 22, 34, 46, 58, 70, 82, 94, 106, 118, 130, 142, 154, 166 ), Month :=  "August"]
  data[month %in% c(   11, 23, 35, 47, 59, 71, 83, 95, 107, 119, 131, 143, 155, 167 ), Month :=  "September"]
  data[month %in% c(   12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168 ), Month :=  "October"]
  data[month %in% c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133, 145, 157, 169 ), Month :=  "November"]
  data[month %in% c(2, 14,     38, 50, 62, 74, 86, 98, 110, 122, 134, 146, 158, 170 ), Month :=  "December"]

  data[month %in% 1:2, Year := 2006]
  data[month %in% 3:14, Year := 2007]
  data[month %in% 15:25, Year := 2008]
  data[month %in% 27:38, Year := 2009]
  data[month %in% 39:50, Year := 2010]
  data[month %in% 51:62, Year := 2011]
  data[month %in% 63:74, Year := 2012]
  data[month %in% 75:86, Year := 2013]
  data[month %in% 87:98, Year := 2014]
  data[month %in% 99:110, Year := 2015]
  data[month %in% 111:122, Year := 2016]
  data[month %in% 123:134, Year := 2017]
  data[month %in% 135:146, Year := 2018]
  data[month %in% 147:158, Year := 2019]
  data[month %in% 159:170, Year := 2020]
  data[month %in% 171:174, Year := 2021]

  data <- data[month %in% start_month:end_month,]

  ################################
  ## SOCIO-ECONOMIC VARIABLES

  data[, gor := factor(gor, levels = c("North East","North West","Yorkshire and The Humber",
                                       "East Midlands","West Midlands","East of England",
                                       "London","South East","South West"))]

  data[, Sex := factor(Sex, levels = c("Men","Women"), labels = c("Male","Female"))]

  data[ , Ageband := c("16-24", "25-34", "35-44", "45-54", "55-64", "65+")[findInterval(Age, c(16, 25, 35, 45, 55, 65, 1000))]]

  data[, Ageband := factor(Ageband, levels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65+"))]

  data[grade %in% c("AB","C1"), grade_2cat := "ABC1"]
  data[grade %in% c("C2","D","E"), grade_2cat := "C2DE"]

  data[work %in% c("HAVE PAID JOB - FULL TIME (30+ HOURS PER WEEK)"),lmstatus := "Employed Full-Time"]
  data[work %in% c("HAVE PAID JOB - PART TIME (8-29 HOURS PER WEEK)",
                   "HAVE PAID JOB - PART TIME (UNDER 8 HOURS PER WEEK)"),lmstatus := "Employed Part-Time"]
  data[work %in% c("SELF-EMPLOYED"),lmstatus := "Self-Employed"]
  data[work %in% c("UNEMPLOYED AND SEEKING WORK"),lmstatus := "Unemployed"]
  data[work %in% c("FULL TIME STUDENT",
                   "STILL AT SCHOOL"),lmstatus := "Education"]
  data[work %in% c("RETIRED"),lmstatus := "Retired"]
  data[work %in% c("NOT WORKING - HOUSEWIFE/HUSBAND",
                   "NOT IN PAID WORK FOR OTHER REASON",
                   "NOT IN PAID WORK BECAUSE OF LONG TERM ILLNESS OR DISABILITY"),lmstatus := "Other Inactive"]

  data[lmstatus %in% c("Employed Full-Time",
                       "Employed Part-Time",
                       "Self-Employed"), lmstatus_3cat := "Employed"]

  data[lmstatus %in% c("Unemployed"), lmstatus_3cat := "Unemployed"]

  data[!(lmstatus %in% c("Employed Full-Time",
                         "Employed Part-Time",
                         "Self-Employed",
                         "Unemployed")), lmstatus_3cat := "Inactive"]

  ############################################################################
  ### GENERATE LOCAL AUTHORITY NAMES AND CLEAN TO MATCH OTHER DATA SOURCES ###

  # read in the mapping from LTLA to UTLA

  data[, LAcode := stringr::str_trim(LAcode)]
  data_out <- merge(data, smkfreediv::localauthorities, by = c("LAcode"), all = TRUE, sort = FALSE)

  ### tidy up so the names and codes match exactly to the tobacco profiles

  # name of Buckinghamshire
  data_out[UTLAname == "Buckinghamshire", UTLAname := "Buckinghamshire UA"]

  # Buckinghamshire code
  data_out[UTLAcode == "E10000002", UTLAcode := "E06000060"]

  # fix mis-match of Bristol naming
  data_out[UTLAcode == "E06000023", UTLAname := "Bristol"]

  # fix mis-match of Kingston upon Hull naming
  data_out[UTLAcode == "E06000010", UTLAname := "Kingston upon Hull"]

  # fix mis-match of Herefordshire naming
  data_out[UTLAcode == "E06000019", UTLAname := "Herefordshire"]

  # Dorset code
  data_out[UTLAcode == "E10000009", UTLAcode := "E06000059"]


  ## drop non-English local authorities
  data_out <- data_out[substring(UTLAcode,1,1) == "E",]

  return(data_out)
}
