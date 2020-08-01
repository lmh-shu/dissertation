### This script saves data from SQL database on schools, teachers and URNs in a data folder
### Created by Livia Hull in September 2019


# Upload libraries --------------------------------------------------------


library(readr)
library(devtools)
library(dplyr)


source("R/functions.R")

# Connect with SQL and save data  -----------------------------------------

read_save_data("queries/get_schools_from_school_table.sql", "data/Schools.rds")

read_save_data("queries/get_teachers_census_from_aggregated_view.sql", "data/Teachers.rds")

read_save_data("queries/get_latestURN _for_all_URN.sql", "data/LatestURNs.rds")

read_save_data("queries/get_turnover.sql", "data/Turnover.rds") 

read_save_data("queries/get_ofsted_scores.sql", "data/Ofsted.rds")

read_save_data("queries/get_qualified_leavers.sql.sql", "data/qualified_leavers.rds")

