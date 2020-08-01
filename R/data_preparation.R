# Load libraries
library(dplyr)
library(lubridate)
library(testthat)

# Source Required Scripts
source("R/functions.R")

# Load data -------------------------------------------------------------
# Load data from KIMs
chains <- read.csv("data/Academies Chains Data.csv", stringsAsFactors = FALSE)
rebrokerages <- read.csv("data/Open Academies Rebrokerage.csv", stringsAsFactors = FALSE)

# Load data from SWFC_Project 
schools <- readRDS("data/Schools.rds") 
teachers <- readRDS("data/Teachers.rds") 
latestURNs <- readRDS("data/LatestURNs.rds") 
turnover <- readRDS("data/Turnover.rds")
ofsted <- readRDS( "data/Ofsted.rds")
qualifiedleavers <- readRDS("data/qualified_leavers.rds")

# Load manual trust input data (Trust ID and Trust Name for closed academies)
manual_input <- read.csv("data/manual_input.csv", stringsAsFactors = FALSE) 

# Initial data cleaning ---------------------------------------------------

# Remove level
latestURNs <- latestURNs %>%
  select(-Level)

# Add previous census year and LatestURN urn to turnover
turnover <- turnover %>%
  left_join(schools, by = c("CensusYear", "LAEstab")) %>%
  # Join Latest urns data
  left_join(latestURNs, by = c("URN")) %>%
  mutate(
    PreviousCensusYear = CensusYear - 1,
    # Add latest URN
    LatestURN = ifelse(is.na(LatestURN), URN, LatestURN)
  ) 


# Expand Schools Table with Academy Categorisation and add LatestURN
schools <- schools %>%
  # Join Latest urns data
  left_join(latestURNs, by = c("URN")) %>%
  mutate(
    # Add academy category
    Academy_type = if_else(grepl("Sponsor", SchoolType), "Sponsored Academies",
                                if_else(grepl("Converter", SchoolType), "Converted Academies",
                                        if_else(grepl("Free", SchoolType), "Converted Academies", "Non-Academies"))),
    # Add latest URN
    LatestURN = ifelse(is.na(LatestURN), URN, LatestURN)
    ) %>% 
  # Add priority for Latest URN for mergers 
  # (if in the same year there are 2 same URNs for schools that merged into one)
  group_by(CensusYear, LatestURN) %>% 
  mutate(
    Primary_Merger_Flag = rank(URN, ties.method = "first")
  ) %>%
  ungroup() %>%
  left_join(ofsted, by = c("CensusYear", "LAEstab")) 
  
# creates a dataset of all academies and free schools from SWFC 
swc_acads <- schools %>%
  # Filter all academies and free schools
  filter(grepl("Acad", SchoolType) | grepl("Free", SchoolType))

# Add LatestURN to chains data
chains <- chains %>%
  rename(URN = URN1) %>% 
  # Join on Latest URNs data
  left_join(latestURNs, by = c("URN")) %>%
  # Create latest urn col
  mutate(LatestURN = ifelse(is.na(LatestURN), URN, LatestURN)) %>%
  # Select only columns of interest
  select(URN, LatestURN, Trust_ID, Trust_Name, Trust_Type) 

# creates a dataset of rebrokered academies with trust id pre-rebrokerage and last year in that trust; 
# Note: 8 are duplicates as they are in process of changing at the date of this analysis
rebrokerages <- rebrokerages %>%
  # Filter those marked as rebrokered
  filter(Re_brokerage_Status == 'Re-brokered') %>%
  # When previous trust id is null then use current trust id instead in case incorrectly inputted
  mutate(previoustrustid = case_when(Previous_Trust_ID == '' ~ Current_Trust_ID, 
                                     Previous_Trust_ID != '' ~ Previous_Trust_ID) ) %>%
  # Calculate last year in previous trust based on date joined new sponsor
  mutate(LastYearInPreviousTrust = date_to_census_year(Date_joined_new_Sponsor)) %>%
  # Select only required columns and de-dupe
  select(URN = Academy_URN, previoustrustid, LastYearInPreviousTrust) %>%
  distinct()

# Append trust to schools table over time ---------------------------------

# Add Trust Ref to SWC Schools over time (Taking into account re-brokerages)
swc_acads_w_trust <- swc_acads %>%
  # Join on Chains data for Current Trust
  left_join(chains, by = c("LatestURN")) %>% 
  # Subset to manageable amount of columns
  select(CensusYear, URN = URN.x, Trust_ID, Trust_Name, Trust_Type) %>%
  # Join Manual Input by URN
  left_join(manual_input, by = c("URN"))  %>% 
  # Create Trust ID and Trust Name based on Chains and Manual input
  mutate(Trust_ID = if_else(!is.na(Trust_ID.x), Trust_ID.x, Trust_ID.y),
         Trust_Name  = if_else(!is.na(Trust_Name.x), Trust_Name.x, Trust_Name.y),
         Trust_Type = if_else(!is.na(Trust_Type.x), Trust_Type.x, Trust_Type.y)) %>%
  # Reduce to manageable amount of columns
  select(CensusYear, URN, Trust_ID, Trust_Name, Trust_Type) %>%
  # Join on rebrokerage data
  left_join(rebrokerages, by = c("URN")) %>% 
  # Fill in Previous trust id's for pre rebrokerage dates
  mutate(TrustID = case_when(CensusYear >= LastYearInPreviousTrust | is.na(LastYearInPreviousTrust) ~ Trust_ID, 
                             TRUE ~ previoustrustid)) %>%
  # Select just columns of interest
  select(CensusYear, URN, TrustID, Trust_Name, Trust_Type) %>%
  # Add in years in trust variable. Note 2010 counted as first year for any
  # academy.
  group_by(URN,TrustID) %>%
  mutate(
    YearInTrust = CensusYear - min(CensusYear, na.rm = TRUE) + 1
  ) %>%
  ungroup()

# TEST all academies have TRUST ID in swc_acads_latest_trust: need to get FALSE
# When run from the command line, tests return NULL if all expectations are met, otherwise it raises an error.
test_that("check all academies have Trust ID", 
          expect_equal(any(is.na(swc_acads_w_trust$TrustID)), FALSE))

# Append back on to Schools Table
schools <- schools %>%
  left_join(swc_acads_w_trust, by = c("CensusYear", "URN"))

# Add school size onto school table (FTE Teachers) ------------------------

# Create school size table
schools_size <- teachers %>%
  group_by(CensusYear, LAEstab) %>%
  summarise(
    School_FTE_Teachers = sum(TOTFTE, na.rm = TRUE)
  ) %>%
  ungroup()

# Append to schools table
schools <- schools %>%
  left_join(schools_size, by = c("CensusYear", "LAEstab")) %>% 
  mutate(
    # Set as factor to ensure order in charts
    School_Size_Teachers = factor(x = case_when(School_FTE_Teachers < 20 ~ "Small",
                                     School_FTE_Teachers < 50 ~ "Medium", 
                                     School_FTE_Teachers < 100 ~ "Large",
                                     TRUE ~ "Very Large"),
                                     levels = c("Small", "Medium", "Large", "Very Large"))
  )


# Add Last Year maintained flag to schools table --------------------------

schools <- schools %>%
  mutate(NextCensusYear = CensusYear + 1) %>%
  # Note we are using the primary_merger_flag so we don't have mergers duplicating schools.
  left_join(schools %>% 
              filter(Primary_Merger_Flag == 1) %>% 
              select(CensusYear, LatestURN, NextYear_Academy_type = Academy_type), 
            by = c("NextCensusYear" = "CensusYear", "LatestURN")) %>%
  mutate(
    LastYearMaintained = case_when(
      Academy_type == "Non-Academies" & NextYear_Academy_type == "Converted Academies" ~ "Non-Academies - Last year before becoming converter",
      Academy_type == "Non-Academies" & NextYear_Academy_type == "Sponsored Academies" ~ "Non-Academies - Last year before becoming sponsored",
      Academy_type == "Non-Academies" ~ "Non-Academies",
      TRUE ~ "Academies"
    )
  ) 

# Join all school level info to teachers table ----------------------------

# create a dataset with all teachers from SWC and School Details (Inc Trust ID and School Type)
teachers <- teachers %>%
  # Join School Table
  left_join(schools, by = c("LAEstab", "CensusYear")) %>%
  # Add qualified leaver column
  left_join(qualifiedleavers, by = c("CensusYear", "StaffMatchingReference")) %>%
  # Subset to columns of interest
  select(CensusYear, StaffMatchingReference, QTStatus, QualifiedLeaverType,
         ContractAgreementType, Post, Leadership, TOTFTE, 
         BasePay, BasePay_FTE, GrossPay, GrossPay_FTE,
         LatestURN, URN, LAEstab, Estab_name, SchoolType, 
         SchoolPhase_Grouped, TrustID, Trust_Name, Trust_Type, LastYearMaintained,
         YearInTrust, Academy_type, School_FTE_Teachers, School_Size_Teachers, OfstedOverallScore)












