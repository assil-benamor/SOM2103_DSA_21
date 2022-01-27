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

# laod aggregated data
data <- read.csv("input/data/aggregation_output_20_01.csv", stringsAsFactors = F)

# create indicator indexes
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
  wash_nc_index = case_when(
    water_access_problem == "yes" & water_access_barriers.waterpoints_disabilities == 1 & water_access_barriers.fetching_activity == 1 & water_access_barriers.insufficient_points == 1 ~ 3,
    water_access_problem == "yes" & water_access_barriers.no_problem == 1 ~ 1
  ),
  
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
  wash_nc_index5 = case_when(
    problems_sanitation_facilities == "yes" &
      hygiene_access_impediments.lack_quant == 1 & hygiene_access_impediments.no_funct_full == 1 &
      hygiene_access_impediments.unhygienic == 1 & hygiene_access_impediments.not_privat == 1 &
      hygiene_access_impediments.no_gender_segr == 1 & hygiene_access_impediments.too_far == 1 &
      hygiene_access_impediments.difficult_reach == 1 & hygiene_access_impediments.dangerous == 1 &
      hygiene_access_impediments.groups_no_access == 1 ~ 3
  ),
  
  ### Protection Critical
  ## protection index 1
  protection_index1 = case_when(
    protection_incidents.armed_violence == 1 & protection_incidents.uxo == 1 & protection_incidents.disappear == 1 & protection_incidents.forced_recruit == 1 ~ 4,
    
    protection_incidents.gbv == 1 & protection_incidents.arrest_detention == 1 & protection_incidents.abduction == 1 &
      protection_incidents.displacement == 1 & protection_incidents.violence_aid_distrib == 1 & protection_incidents.exploit_abuse_access_aid == 1 &
      protection_incidents.unaccomp_child == 1 & protection_incidents.destruction_property == 1 ~ 3,
    
    protection_incidents.illegal_tax == 1 & protection_incidents.inter_communal == 1 & protection_incidents.land_grabbing == 1 &
      protection_incidents.denied_access_justice == 1 ~ 2
  ),
  
  ## protection index 2
  protection_index2 = case_when(
    insecure_areas.in_shelter == 1 & insecure_areas.water_point == 1 & insecure_areas.latrines == 1 & insecure_areas.bathing_areas == 1 &
      insecure_areas.schools == 1 & insecure_areas.way_to_school == 1 ~ 4,
    
    insecure_areas.outside_settlement == 1 & insecure_areas.markets == 1 & insecure_areas.way_to_market == 1 &
      insecure_areas.health_centres == 1 & insecure_areas.nutrition_centres == 1 & insecure_areas.humanitarian_aid == 1 ~ 3,
    
    insecure_areas.no_problems == 1 ~ 1
  ),
  
  ### Protection Non-Critical indicators
  ## protection nc index 1
  
  
  ## protection nc index 2
  protection_nc_index2 = case_when(
    protection_childfriendlyspace == "no" ~ 3,
    protection_childfriendlyspace == "yes" ~ 2
  ),
  
  ## protection nc index 3
  protection_nc_index3 = case_when(
    protection_restrictions_day == "no" ~ 3,
    protection_restrictions_day == "yes" ~ 2
  ),
  
  ## protection nc index4
  protection_nc_index4 = case_when(
    protection_restrictions_night == "no" ~ 2,
    protection_restrictions_night == "yes" ~ 3
  ),
  
  ## protection nc index5
  protection_nc_index5 = case_when(
    support_access_impediments.minorities == 1 ~ 3,
    support_access_impediments.women == 1 & support_access_impediments.children == 1 & support_access_impediments.elders == 1 & support_access_impediments.disabled == 1 & support_access_impediments.marginalised == 1 & support_access_impediments.no_impediments == 1 ~ 2          
  ),
  
  ### Education Non-Critical Index
  ## education nc index 1
  education_nc_index1 = case_when(
    education_facilities.no_available == 1 ~ 1,
    education_facilities.primary == 1 & education_facilities.secondary == 1 & education_facilities.quoranic == 1 & education_facilities.basic_edu == 1 ~ 0
  ),
  
  ## education nc index 2
  education_nc_index2 = case_when(
    number_schools_opened == 0 ~ 1,
    number_schools_opened >= 1 ~ 0
  ),
  
  ## education nc index 3
  education_nc_index3 = case_when(
    education_facilities_segregated %in% c("none", "few") ~ 1,
    education_facilities_segregated %in% c("many", "some", "all") ~ 0
  ),
  
  ## education nc index 4
  education_nc_index4 = case_when(
    education_facilities_fences %in% c("none", "few") ~ 1,
    education_facilities_fences %in% c("many", "some", "all") ~ 0
  ),
  
  ## education nc index 5
  education_nc_index5 = case_when(
    education_access_distance_min == "more_60" ~ 1,
    education_access_distance_min %in% c("1230", "less_15", "3160") ~ 0
  ),
  
  ## education nc index 6
  education_nc_index6 = case_when(
    boys_5_12 %in% c("few", "none") ~ 1,
    boys_5_12 %in% c("many", "some", "all") ~ 0
  ),
  
  ## education nc index 7
  education_nc_index7 = case_when(
    girls_5_12 %in% c("few", "none") ~ 1,
    girls_5_12 %in% c("many", "some", "all") ~ 0
  ),
  
  ## education nc index 8
  education_nc_index8 = case_when(
    boys_13_17 %in% c("few", "none") ~ 1,
    boys_13_17 %in% c("many", "some", "all") ~ 0
  ),
  
  ## education nc index 9
  education_nc_index9 = case_when(
    girls_13_17 %in% c("few", "none") ~ 1,
    girls_13_17 %in% c("many", "some", "all") ~ 0
  ),
  
  ### Education Critical Indicator
  ## education index 1
  education_inex = case_when(
    education_barriers_boys.child_recruited_ag == 1 & education_barriers_boys.displacement_conflict == 1 ~ 4,
    
    education_barriers_boys.security_concerns == 1 & education_barriers_boys.child_lack_documentation == 1 & 
      education_barriers_boys.costs == 1 & education_barriers_boys.child_pycho_distress & education_barriers_boys.help_at_home == 1 &
      education_barriers_boys.work_outside_home == 1 & education_barriers_boys.marriage_pregnant == 1 & education_barriers_boys.flood == 1 ~ 3,
    
    education_barriers_boys.far_away == 1 & education_barriers_boys.closed == 1 & education_barriers_boys.poor_infrastructure == 1 &
      education_barriers_boys.lack_quali_staff == 1 & education_barriers_boys.no_wash_at_school == 1 &
      education_barriers_boys.no_gender_separ == 1 & education_barriers_boys.help_at_home == 1 ~ 2,
    
    education_barriers_boys.no_problem == 1 & education_barriers_boys.language == 1 & education_barriers_boys.parents_no_value_edu == 1 &
      education_barriers_boys.parents_no_approve_curric == 1 & education_barriers_boys.cultural_beliefs == 1 & education_barriers_boys.no_aware_education_opportunities == 1 ~ 1
  ),
  
  ## education index 2
  education_index2 = case_when(
    education_barriers_girls.child_recruited_ag == 1 & education_barriers_girls.displacement_conflict == 1 ~ 4,
    
    education_barriers_girls.security_concerns == 1 & education_barriers_girls.child_lack_documentation == 1 & 
      education_barriers_girls.costs == 1 & education_barriers_girls.child_pycho_distress & education_barriers_girls.help_at_home == 1 &
      education_barriers_girls.work_outside_home == 1 & education_barriers_girls.marriage_pregnant == 1 & education_barriers_girls.flood == 1 ~ 3,
    
    education_barriers_girls.far_away == 1 & education_barriers_girls.closed == 1 & education_barriers_girls.poor_infrastructure == 1 &
      education_barriers_girls.lack_quali_staff == 1 & education_barriers_girls.no_wash_at_school == 1 &
      education_barriers_girls.no_gender_separ == 1 & education_barriers_girls.help_at_home == 1 ~ 2,
    
    education_barriers_girls.no_problem == 1 & education_barriers_girls.language == 1 & education_barriers_girls.parents_no_value_edu == 1 &
      education_barriers_girls.parents_no_approve_curric == 1 & education_barriers_girls.cultural_beliefs == 1 & education_barriers_girls.no_aware_education_opportunities == 1 ~ 1
  )
  
)

