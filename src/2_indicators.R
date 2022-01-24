## DSA-2021 LSG Indicators Coding
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
p_load(rio,
       tidyverse,
       koboquest,
       crayon,
       hypegrammaR,
       xlsx,
       composr)

data <- read.csv("input/data/aggregation_output_20_01.csv", stringsAsFactors = F)

data_indicators <- data %>% mutate(
  ### WASH Critical
  ## WASH Index 1
  wash_index1 = case_when(
    water_sources_primary %in% c("river") ~ 4,
    water_sources_primary %in% c("unprot_well", "berkad") ~ 3,
    water_sources_primary %in% c("borehole_pump", "piped", "prot_well_handpump", "prot_well_no_handpump", "vendors_shop",
                                 "water_kiosk", "water_tank_tap", "water_trucking_distrib") ~ 1
  ),
  
  ## WASH Index 2
  wash_index2 = case_when(
    water_access_distance_min == "more_60" ~ 4,
    water_access_distance_min == "3160" ~ 3,
    water_access_distance_min == "1530" ~ 2,
    water_access_distance_min == "less_15" ~ 1
  ),
  
  ## WASH Index 3
  wash_index3 = case_when(
    sanitation_access_distance_min == "more_60" ~ 4,
    sanitation_access_distance_min == "3160" ~ 3,
    sanitation_access_distance_min == "1530" ~ 2,
    sanitation_access_distance_min == "less_15" ~ 1
  ),
  
  ## WASH Index 4
  wash_index4 = case_when(
    hygiene_handwashingfacilities == "all" ~ 4,
    hygiene_handwashingfacilities == "many" ~ 3,
    hygiene_handwashingfacilities == "some" ~ 2,
    hygiene_handwashingfacilities == "few" ~ 1
  ),
  
  ### WASH Non-Critical
  ## wash_nc_index1
  
  
  ## wash_nc_index2
  wash_nc_index2 = case_when(
    sanitation_solidwastedisposal.disposed_open_grounds == 1 | sanitation_solidwastedisposal.burnt_open_spaces == 1 | sanitation_solidwastedisposal.not_managed == 1 ~ 3,
    sanitation_solidwastedisposal.buried_in_pit == 1 ~ 1
  ),
  
  ## wash_nc_index3
  wash_nc_index3 = case_when(
    water_treatment_proportion %in% c("some", "many", "all") ~ 3,
    water_treatment_proportion == "none" ~ 1
  ),
  
  ## wash_nc_index4
  wash_nc_index4 = case_when(
    latrines_accessible_pwd %in% c("some", "many", "all") ~ 3,
    latrines_accessible_pwd == "none" ~ 1
  ),
  
  ## wash_nc_index5
  
  
)

