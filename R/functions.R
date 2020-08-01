
library(dplyr)
library(ggplot2)
library(scales)

# # read data from SQL  ---------------------------------------------------
read_save_data <- function(sql_script, saved_file){
  
  database <- sql_conn_string("3DCPRI-PDB16\\ACSQLS", "SWFC_Project")
  
  con <- dbConnect(odbc(), .connection_string = database)
  
  sqlString <- read_sql_script(sql_script)
  
  dataset <- dbGetQuery(con, sqlString)
  
  dbDisconnect(con)
  
  saveRDS(dataset, saved_file)
  
}



# # transforms a date into a census year ----------------------------------
date_to_census_year <- function(date) {
  date_formatted <- as.POSIXct(date, format = "%d/%m/%Y")
  case_when(date_formatted > ymd("2019-11-07") ~ 2019,
            between(date_formatted, ymd("2018-11-08"), ymd("2019-11-06") ) ~ 2018,
            between(date_formatted, ymd("2017-11-02"), ymd("2018-11-07") ) ~ 2017,
            between(date_formatted, ymd("2016-11-04"), ymd("2017-11-01") ) ~ 2016,
            between(date_formatted, ymd("2015-11-06"), ymd("2016-11-03") ) ~ 2015,
            between(date_formatted, ymd("2014-11-07"), ymd("2015-11-05") ) ~ 2014,
            between(date_formatted, ymd("2013-11-08"), ymd("2014-11-06") ) ~ 2013,
            between(date_formatted, ymd("2012-11-07"), ymd("2013-11-07") ) ~ 2018,
            between(date_formatted, ymd("2011-11-04"), ymd("2012-11-06") ) ~ 2017,
            between(date_formatted, ymd("2010-11-05"), ymd("2011-11-03") ) ~ 2016,
            between(date_formatted, ymd("2009-11-05"), ymd("2010-11-04") ) ~ 2015,
            month(date_formatted) == 12 | ( month(date_formatted) == 11 & day(date_formatted) >= 5 ) ~ isoyear(date_formatted),
            month(date_formatted) <= 10 | ( month(date_formatted) == 11 & day(date_formatted) < 5 ) ~ isoyear(date_formatted)-1
  )
}

# # create not in opeator -------------------------------------------------
'%!in%' <- function(x,y){!('%in%'(x,y))}


# create percent function -------------------------------------------------

percent <- function(value, dp = 1, suffix = "%"){
  paste0(round(100 * value, dp), suffix)
}

