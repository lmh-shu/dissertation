library(dplyr)
library(ggplot2)
library(tidyr)

source("R/data_preparation.R")

# Calculate turnover and leavers for each teacher -----------------------------------------------------------------------
turn_wast_teachers <- teachers %>% 
  # Exclude Centrally Employed  
  filter(SchoolType != "Centrally Employed") %>%
  # join turnover data
  left_join(turnover, by = c("CensusYear" = "PreviousCensusYear", "StaffMatchingReference")) %>% 
  # take out 2018 as it is last census year so turnover does not exist
  filter(CensusYear < 2018) %>%
  # keep only columns needed 
  select(CensusYear, 
         StaffMatchingReference, 
         QTStatus,
         LAEstab.x,
         LAEstab.y,
         LatestURN.x,
         LatestURN.y,
         LastYearMaintained,
         School_Type = Academy_type, 
         SchoolPhase_Grouped = SchoolPhase_Grouped.x,
         School_FTE_Teachers,
         School_Size_Teachers,
         TrustID,
         Trust_Type,
         YearInTrust, 
         OfstedOverallScore,
         TOTFTE,
         Leadership,
         ContractAgreementType,
         BasePay, BasePay_FTE, GrossPay, GrossPay_FTE,
         QualifiedLeaverType) %>%
  mutate(LatestURN.x = ifelse(is.na(LatestURN.x), 0, LatestURN.x),
         LatestURN.y = ifelse(is.na(LatestURN.y), 0, LatestURN.y),
         QualifiedLeaverType = ifelse(is.na(QualifiedLeaverType), "NA", QualifiedLeaverType)
        ) %>%
  # flag in columns what teachers moved to other schools and what teachers left system
  mutate(
    LatestURN = LatestURN.x,
    Turnover = ifelse(LatestURN.x != LatestURN.y, 1, 0),
    Wastage = ifelse(QualifiedLeaverType == "Wastage", 1, 0)
    ) %>%
  select(-QualifiedLeaverType)


# Aggregate to school level -----------------------------------------------

turn_wast_schools <- turn_wast_teachers %>%  
  group_by(CensusYear, 
           LatestURN,
           LastYearMaintained,
           School_Type,
           SchoolPhase_Grouped,
           School_FTE_Teachers,
           School_Size_Teachers, 
           TrustID, 
           Trust_Type,
           YearInTrust,
           OfstedOverallScore) %>%
  summarise(FTE = sum(TOTFTE, na.rm = TRUE),
            Turnover = sum(Turnover * TOTFTE, na.rm = TRUE),
            Wastage = sum(Wastage * TOTFTE , na.rm = TRUE)) %>%
  ungroup() %>%
  # calculate turnover and leavers as percentages of total headcount
  mutate(
    Per_Turnover = round(Turnover/FTE*100, digits = 2),
    Per_Wastage = round(Wastage/FTE*100, digits = 2)
    )

# Reduce for regression -----------------------------------------------


