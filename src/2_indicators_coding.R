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
data <- read.csv("output/Aggregation/DSA_aggregated_data.csv", stringsAsFactors = F)

# create indicator indexes
data_indicators <- data %>% mutate(
  #### WASH Critical  #### 
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
    hygiene_handwashingfacilities == "all" ~ 1,
    hygiene_handwashingfacilities == "many" ~ 2,
    hygiene_handwashingfacilities == "some" ~ 3,
    hygiene_handwashingfacilities %in% c("few", "none") ~ 4
  ),
  
  #### WASH Non-Critical  #### 
  ## wash_nc_index1
  wash_nc_index = case_when(
    water_access_problem == "no" | water_access_barriers.no_problem == 1 ~ 0,
    water_access_problem == "yes" | water_access_barriers.waterpoints_disabilities == 1 | water_access_barriers.fetching_activity == 1 | water_access_barriers.insufficient_points == 1 ~ 1,
    is.na(water_access_problem) ~ 0
  ),
  
  ## wash_nc_index2
  wash_nc_index2 = case_when(
    sanitation_solidwastedisposal.disposed_open_grounds == 1 | sanitation_solidwastedisposal.burnt_open_spaces == 1 | sanitation_solidwastedisposal.not_managed == 1 ~ 1,
    sanitation_solidwastedisposal.buried_in_pit == 1 | sanitation_solidwastedisposal.dnk == 1 | sanitation_solidwastedisposal.other == 1 ~ 0
  ),
  
  ## wash_nc_index3
  wash_nc_index3 = case_when(
    water_sources_primary %in% c("berkad", "river", "unprot_well") & water_treatment_proportion %in% c("none", "few") ~ 1,
    water_sources_primary %in% c("berkad", "river", "unprot_well") & water_treatment_proportion %in% c("some", "many", "all") ~ 0,
    is.na(water_sources_primary) ~ 0
  ),
  
  ## wash_nc_index4
  wash_nc_index4 = case_when(
    latrines_accessible_pwd %in% c("none", "few") ~ 1,
    latrines_accessible_pwd %in% c("some", "many", "all") ~ 0,
    is.na(latrines_accessible_pwd) ~ 0
  ),
  
  ## wash_nc_index5
  wash_nc_index5 = case_when(
    problems_sanitation_facilities == "yes" &
      hygiene_access_impediments.lack_quant == 1 | hygiene_access_impediments.no_funct_full == 1 |
      hygiene_access_impediments.unhygienic == 1 | hygiene_access_impediments.not_privat == 1 |
      hygiene_access_impediments.no_gender_segr == 1 | hygiene_access_impediments.too_far == 1 |
      hygiene_access_impediments.difficult_reach == 1 | hygiene_access_impediments.dangerous == 1 |
      hygiene_access_impediments.groups_no_access == 1 ~ 1,
    
    problems_sanitation_facilities == "no" ~ 0
  ),
  
  #### Protection Critical  #### 
  ## protection index 1
  protection_index1 = case_when(
    protection_incidents.armed_violence == 1 | protection_incidents.uxo == 1 | protection_incidents.disappear == 1 | protection_incidents.forced_recruit == 1 ~ 4,
    
    protection_incidents.gbv == 1 | protection_incidents.arrest_detention == 1 | protection_incidents.abduction == 1 |
      protection_incidents.displacement == 1 | protection_incidents.violence_aid_distrib == 1 | protection_incidents.exploit_abuse_access_aid == 1 |
      protection_incidents.unaccomp_child == 1 | protection_incidents.destruction_property == 1 ~ 3,
    
    protection_incidents.illegal_tax == 1 | protection_incidents.inter_communal == 1 | protection_incidents.land_grabbing == 1 |
      protection_incidents.denied_access_justice == 1 ~ 2,
    
    protection_incidents.dnk == 1 | protection_incidents.pnta == 1 | protection_incidents.other == 1 ~ 1
  ),
  
  ## protection index 2
  protection_index2 = case_when(
    insecure_areas.in_shelter == 1 | insecure_areas.water_point == 1 | insecure_areas.latrines == 1 | insecure_areas.bathing_areas == 1 |
      insecure_areas.schools == 1 | insecure_areas.way_to_school == 1 ~ 4,
    
    insecure_areas.outside_settlement == 1 | insecure_areas.markets == 1 | insecure_areas.way_to_market == 1 |
      insecure_areas.health_centres == 1 | insecure_areas.nutrition_centres == 1 | insecure_areas.humanitarian_aid == 1 ~ 3,
    
    insecure_areas.no_problems == 1 | insecure_areas.dnk == 1 | insecure_areas.pnta == 1 | insecure_areas.other == 1 ~ 1
  ),
  
  #### Protection Non-Critical indicators  #### 
  ## protection nc index 1
  protection_nc_index1 = case_when(
    protection_womenspace == "no" ~ 1,
    protection_womenspace == "yes" ~ 0
  ),
  
  ## protection nc index 2
  protection_nc_index2 = case_when(
    protection_childfriendlyspace == "no" ~ 1,
    protection_childfriendlyspace == "yes" ~ 0
  ),
  
  ## protection nc index 3
  protection_nc_index3 = case_when(
    protection_restrictions_day == "no" ~ 0,
    protection_restrictions_day == "yes" ~ 1
  ),
  
  ## protection nc index4
  protection_nc_index4 = case_when(
    protection_restrictions_night == "no" ~ 0,
    protection_restrictions_night == "yes" ~ 1
  ),
  
  ## protection nc index5
  protection_nc_index5 = case_when(
    support_access_impediments.minorities == 1 ~ 1,
    support_access_impediments.women == 1 | support_access_impediments.children == 1 | support_access_impediments.elders == 1 | support_access_impediments.disabled == 1 | support_access_impediments.marginalised == 1 | support_access_impediments.no_impediments == 1 ~ 0          
  ),
  
  #### Education Non-Critical Index  #### 
  ## education nc index 1
  education_nc_index1 = case_when(
    education_facilities.no_available == 1 ~ 1,
    education_facilities.primary == 1 | education_facilities.secondary == 1 | education_facilities.quoranic == 1 | education_facilities.basic_edu == 1 ~ 0
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
  
  #### Education Critical Indicator  #### 
  ## education index 1
  education_index1 = case_when(
    education_barriers_boys.child_recruited_ag == 1 | education_barriers_boys.displacement_conflict == 1 ~ 4,
    
    education_barriers_boys.security_concerns == 1 | education_barriers_boys.child_lack_documentation == 1 | 
      education_barriers_boys.costs == 1 | education_barriers_boys.child_pycho_distress |
      education_barriers_boys.work_outside_home == 1 | education_barriers_boys.marriage_pregnant == 1 | education_barriers_boys.flood == 1 ~ 3,
    
    education_barriers_boys.far_away == 1 | education_barriers_boys.closed == 1 | education_barriers_boys.poor_infrastructure == 1 |
      education_barriers_boys.lack_quali_staff == 1 | education_barriers_boys.no_wash_at_school == 1 | education_barriers_boys.overcrowded == 1 |
      education_barriers_boys.no_gender_separ == 1 | education_barriers_boys.help_at_home == 1 ~ 2,
    
    education_barriers_boys.no_problem == 1 | education_barriers_boys.language == 1 | education_barriers_boys.parents_no_value_edu == 1 |
      education_barriers_boys.parents_no_approve_curric == 1 | education_barriers_boys.cultural_beliefs == 1 | education_barriers_boys.no_aware_education_opportunities == 1 ~ 1
  ),
  
  ## education index 2
  education_index2 = case_when(
    education_barriers_girls.child_recruited_ag == 1 | education_barriers_girls.displacement_conflict == 1 ~ 4,
    
    education_barriers_girls.security_concerns == 1 | education_barriers_girls.child_lack_documentation == 1 | 
      education_barriers_girls.costs == 1 | education_barriers_girls.child_pycho_distress |
      education_barriers_girls.work_outside_home == 1 | education_barriers_girls.marriage_pregnant == 1 | education_barriers_girls.flood == 1 ~ 3,
    
    education_barriers_girls.far_away == 1 | education_barriers_girls.closed == 1 | education_barriers_girls.poor_infrastructure == 1 |
      education_barriers_girls.lack_quali_staff == 1 | education_barriers_girls.no_wash_at_school == 1 | education_barriers_girls.overcrowded == 1 |
      education_barriers_girls.no_gender_separ == 1 | education_barriers_girls.help_at_home == 1 ~ 2,
    
    education_barriers_girls.no_problem == 1 | education_barriers_girls.language == 1 | education_barriers_girls.parents_no_value_edu == 1 |
      education_barriers_girls.parents_no_approve_curric == 1 | education_barriers_girls.cultural_beliefs == 1 | education_barriers_girls.no_aware_education_opportunities == 1 ~ 1
  ),
  
  #### Health Critical Indicators  #### 
  ## health_index1
  health_index1 = case_when(
    health_access_distance_min == "more_60" ~ 4,
    health_access_distance_min == "3160" ~ 3,
    health_access_distance_min == "1530" ~ 2,
    health_access_distance_min == "less_15" ~ 1
  ),
  
  ## health index2
  health_index2 = case_when(
    rowSums(across(c(male_health_problems.malaria,
                     male_health_problems.fever,
                     male_health_problems.awd_cholera,
                     male_health_problems.resp_problems,
                     male_health_problems.malnutrition,
                     male_health_problems.gastrointernal,
                     male_health_problems.injuries,
                     male_health_problems.measles,
                     male_health_problems.no_health_issues,
                     male_health_problems.no_health_issues)), na.rm = T) >= 4 ~ 4,
    
    rowSums(across(c(male_health_problems.malaria,
                     male_health_problems.fever,
                     male_health_problems.awd_cholera,
                     male_health_problems.resp_problems,
                     male_health_problems.malnutrition,
                     male_health_problems.gastrointernal,
                     male_health_problems.injuries,
                     male_health_problems.measles,
                     male_health_problems.no_health_issues,
                     male_health_problems.no_health_issues)), na.rm = T) == 3  ~ 3,
    
    rowSums(across(c(male_health_problems.malaria,
                     male_health_problems.fever,
                     male_health_problems.awd_cholera,
                     male_health_problems.resp_problems,
                     male_health_problems.malnutrition,
                     male_health_problems.gastrointernal,
                     male_health_problems.injuries,
                     male_health_problems.measles,
                     male_health_problems.no_health_issues,
                     male_health_problems.no_health_issues)), na.rm = T) == 2 ~ 2,
    
    rowSums(across(c(male_health_problems.malaria,
                     male_health_problems.fever,
                     male_health_problems.awd_cholera,
                     male_health_problems.resp_problems,
                     male_health_problems.malnutrition,
                     male_health_problems.gastrointernal,
                     male_health_problems.injuries,
                     male_health_problems.measles,
                     male_health_problems.no_health_issues,
                     male_health_problems.no_health_issues)), na.rm = T) == 1 ~ 1
  ),
  
  ## health index3
  health_index3 = case_when(
    rowSums(across(c(female_health_problems.malaria,
                     female_health_problems.fever,
                     female_health_problems.awd_cholera,
                     female_health_problems.resp_problems,
                     female_health_problems.malnutrition,
                     female_health_problems.gastrointernal,
                     female_health_problems.injuries,
                     female_health_problems.measles,
                     female_health_problems.no_health_issues)), na.rm = T) >= 4 ~ 4,
    
    rowSums(across(c(female_health_problems.malaria,
                     female_health_problems.fever,
                     female_health_problems.awd_cholera,
                     female_health_problems.resp_problems,
                     female_health_problems.malnutrition,
                     female_health_problems.gastrointernal,
                     female_health_problems.injuries,
                     female_health_problems.measles,
                     female_health_problems.no_health_issues)), na.rm = T) == 3 ~ 3,
    
    rowSums(across(c(female_health_problems.malaria,
                     female_health_problems.fever,
                     female_health_problems.awd_cholera,
                     female_health_problems.resp_problems,
                     female_health_problems.malnutrition,
                     female_health_problems.gastrointernal,
                     female_health_problems.injuries,
                     female_health_problems.measles,
                     female_health_problems.no_health_issues)), na.rm = T) == 2 ~ 2,
    
    rowSums(across(c(female_health_problems.malaria,
                     female_health_problems.fever,
                     female_health_problems.awd_cholera,
                     female_health_problems.resp_problems,
                     female_health_problems.malnutrition,
                     female_health_problems.gastrointernal,
                     female_health_problems.injuries,
                     female_health_problems.measles,
                     female_health_problems.no_health_issues)), na.rm = T) == 1 ~ 1
  ),
  
  ## health index4
  health_index4 = case_when(
    health_women_unskilledhealthpersonnel == "all" ~ 4,
    health_women_unskilledhealthpersonnel %in% c("some", "many") ~ 3,
    health_women_unskilledhealthpersonnel == "few" ~ 2,
    health_women_unskilledhealthpersonnel == "none" ~ 1
  ),
  
  #### Health Non-Critical Indicators  #### 
  ## health nc index1
  health_nc_index1 = case_when(
    health_facilities.no_health_facility == 1 ~ 1,
    health_facilities.first_aid_post == 1 | health_facilities.pharmacy == 1 | health_facilities.district_hospital == 1 | health_facilities.mobile_clinic == 1 | health_facilities.private_clinic == 1 | health_facilities.ngo_clinic == 1 | health_facilities.govt_clinic == 1 ~ 0
  ),
  
  ## health nc index2
  health_nc_index2 = case_when(
    health_services.none == 1 | health_services.dnk == 1 | health_services.other == 1 ~ 1,
    health_services.prim_hc == 1 | health_services.vaccinations == 1 | health_services.child_hc == 1 | health_services.maternal_hc == 1 | health_services.nutrition_services == 1 | health_services.hiv_counsell_testing == 1 | health_services.mental_health_services == 1 ~ 0
  ),
  
  ## health nc index3
  health_nc_index3 = case_when(
    male_sickness %in% c("some", "many", "all") ~ 1,
    male_sickness %in% c("none", "few") ~ 0
  ),
  
  ## health nc index4
  health_nc_index4 = case_when(
    female_sickness %in% c("some", "many", "all") ~ 1,
    female_sickness %in% c("none", "few") ~ 0
  ),
  
  ## health nc index5 - male_wounds
  health_nc_index5 = case_when(
    male_wounds %in% c("some", "many", "all") ~ 1,
    male_wounds %in% c("none", "few") ~ 0
  ),
  
  ## health nc index6 - female_wounds
  health_nc_index6 = case_when(
    female_wounds %in% c("some", "many", "all") ~ 1,
    female_wounds %in% c("none", "few") ~ 0
  ),
  
  ## health nc index7 - male_disabilities
  health_nc_index7 = case_when(
    male_disabilities %in% c("some", "many", "all") ~ 1,
    male_disabilities %in% c("none", "few") ~ 0
  ),
  
  ## health nc index8 - female_disabilities
  health_nc_index8 = case_when(
    female_disabilities %in% c("some", "many", "all") ~ 1,
    female_disabilities %in% c("none", "few") ~ 0
  ),
  
  ## health nc index9 - male_mental_health
  health_nc_index9 = case_when(
    male_mental_health %in% c("some", "many", "all") ~ 1,
    male_mental_health %in% c("none", "few") ~ 0
  ),
  
  ## health nc index10 - female_mental_health
  health_nc_index10 = case_when(
    female_mental_health %in% c("some", "many", "all") ~ 1,
    female_mental_health %in% c("none", "few") ~ 0
  ),
  
  ## health nc index11 - adeqaute_health_men
  health_nc_index11 = case_when(
    adeqaute_health_men %in% c("some", "many", "all") ~ 1,
    adeqaute_health_men %in% c("none", "few") ~ 0
  ),
  
  ## health nc index12 - adeqaute_health_women
  health_nc_index12 = case_when(
    adeqaute_health_women %in% c("some", "many", "all") ~ 1,
    adeqaute_health_women %in% c("none", "few") ~ 0
  ),
  
  ## health nc index13 - health_barriers
  health_nc_index13 = case_when(
    health_barriers.cost == 1 | health_barriers.no_qualified == 1 | health_barriers.not_open == 1 | health_barriers.far_away == 1 | health_barriers.refuse_treatment == 1 | health_barriers.no_medicine == 1 | health_barriers.no_treatment_avail == 1 | health_barriers.pwd_excluded == 1 ~ 1,
    health_barriers.no_problem == 1 ~ 0
  ),
  
  #### Nutrition Non-Critical Indicators  #### 
  ## nutrition nc index1
  nutrition_nc_index1 = case_when(
    nutrition_distributions.none == 1 | nutrition_distributions.dnk == 1 ~ 1,
    nutrition_distributions.muac_tape == 1 | nutrition_distributions.plumpy == 1 | nutrition_distributions.super_cereal_plus == 1 | nutrition_distributions.therap_dairy == 1 ~ 0
  ),
  
  ## nutrition nc index2
  nutrition_nc_index2 = case_when(
    nutrition_access_distance_min == "more_60" ~ 1,
    nutrition_access_distance_min %in% c("less_15", "3160", "31_60", "1530") ~ 0
  ),
  
  ## nutrition nc index3
  nutrition_nc_index3 = case_when(
    rowSums(across(c(nutrition_services.cost,
                     nutrition_services.no_qualified,
                     nutrition_services.documents,
                     nutrition_services.no_referral,
                     nutrition_services.not_open,
                     nutrition_services.far_away,
                     nutrition_services.refuse_treatment_some_groups,
                     nutrition_services.staff_disrespectful,
                     nutrition_services.refuse_treatment,
                     nutrition_services.no_medicine,
                     nutrition_services.no_treatment_avail,
                     nutrition_services.pwd_excluded)), na.rm = T) >= 3 ~ 1,
    
    rowSums(across(c(nutrition_services.cost,
                     nutrition_services.no_qualified,
                     nutrition_services.documents,
                     nutrition_services.no_referral,
                     nutrition_services.not_open,
                     nutrition_services.far_away,
                     nutrition_services.refuse_treatment_some_groups,
                     nutrition_services.staff_disrespectful,
                     nutrition_services.refuse_treatment,
                     nutrition_services.no_medicine,
                     nutrition_services.no_treatment_avail,
                     nutrition_services.pwd_excluded)), na.rm = T) <= 2 ~ 0
    
  ),
  
  #### NFI Critical Indicators  #### 
  ## nfi_inxex1
  nfi_index1 = case_when(
    none == "all" ~ 4,
    none %in% c("some", "many") ~ 3,
    none == "few" ~ 2,
    none == "none" ~ 1
  ),
  
  #### NFI Non-Critical Indicators
  ## nfi nc index1
  nfi_nc_index1 = case_when(
    nfi_access == "no" | nfi_access_dist_min_int > 60 ~ 1,
    nfi_access == "yes" | nfi_access_dist_min_int < 60 ~ 0
  ),
  
  ## nfi nc index2
  nfi_nc_index2 = case_when(
    rowSums(across(c(nfi_items_available.sleep_mats,
                     nfi_items_available.plastic_sheets,
                     nfi_items_available.blankets,
                     nfi_items_available.jerry_cans_buckets,
                     nfi_items_available.cooking_utensils,
                     nfi_items_available.mosquito_nets,
                     nfi_items_available.solar_lamp)), na.rm = T) <= 4 ~ 1,
    
    rowSums(across(c(nfi_items_available.sleep_mats,
                     nfi_items_available.plastic_sheets,
                     nfi_items_available.blankets,
                     nfi_items_available.jerry_cans_buckets,
                     nfi_items_available.cooking_utensils,
                     nfi_items_available.mosquito_nets,
                     nfi_items_available.solar_lamp)), na.rm = T) >= 5 ~ 0
  ),
  
  ## nfi nc index3
  nfi_nc_index3 = case_when(
    cccm_idps_arrival %in% c("fourtosixmonths", "morethansixmonths") | support.shelter_kit == 0 ~ 1,
    cccm_idps_arrival %in% c("lessthanonemonth", "onetothreemonths") | support.shelter_kit == 1 ~ 0,
    is.na(cccm_idps_arrival) ~ 0
  ),
  
  ## nfi nc index4
  nfi_nc_index4 = case_when(
    cccm_idps_arrival %in% c("fourtosixmonths", "morethansixmonths") & support.nfi_kit == 0 ~ 1,
    cccm_idps_arrival %in% c("lessthanonemonth", "onetothreemonths") & support.nfi_kit == 1 ~ 0,
    is.na(cccm_idps_arrival) ~ 0
    
  ),
  
  ## nfi nc index5
  nfi_nc_index5 = case_when(
    shelter_publiclighting == "no" ~ 1,
    shelter_publiclighting == "yes" ~ 0
  ),
  
  ## nfi nc index6
  nfi_nc_index6 = case_when(
    shelter_types %in% c("buul", "tent", "timber_plastic_cgi", "shelter_kit") ~ 1,
    shelter_types %in% c("cgi_wall_roof", "mud_stick_cgi", "plywood_cgi", "stone_brick_cgi1", "stone_brick_cgi2") ~ 0
  ),
  
  #### HLP Critical Indicators  #### 
  ## hlp index1
  hlp_index1 = case_when(
    housing_property_incidences.confiscation_property == 1 | housing_property_incidences.illegal_occupation == 1 ~ 4,
    housing_property_incidences.encroachment_boundary_disputes == 1 ~ 3,
    housing_property_incidences.damaged_inadequate_accomodation == 1 ~ 2,
    housing_property_incidences.dnk == 1 ~ 1
  ),
  
  ## hlp index2
  hlp_index2 = case_when(
    evictions_notice == "yes" ~ 4,
    evictions_notice == "no" ~ 1
  ),
  
  ## hlp index3
  hlp_index3 = case_when(
    rate_likelihood_eviction == "very_high" ~ 4,
    rate_likelihood_eviction == "moderate_moderate" ~ 3,
    rate_likelihood_eviction %in% c("low", "pnta") ~ 1
  ),
  
  #### HLP Non-Critical Indicators  #### 
  ## hlp nc index1
  hlp_nc_index1 = case_when(
    evictions_landowner == "no_owner" ~ 1,
    evictions_landowner %in% c("fed_govt", "local_authority_govt", "mixed", "private_owner", "pnta") ~ 0
  ),
  
  ## hlp nc index2
  hlp_nc_index2 = case_when(
    evictions_tenureagreement == "no" | type_land_agreement == "oral" ~ 1,
    type_land_agreement == "written" ~ 0,
    is.na(type_land_agreement) ~ 0
  ),
  
  #### Food Security Critical Indicators  #### 
  ## food security index1 - the indicator is removed
  
  
  ## food_security_index2
  food_sec_index2 = case_when(
    foodsecurity_access_barriers.sec_issues == 1 | foodsecurity_access_barriers.harassment_way_distribution_site == 1 ~ 4,
    foodsecurity_access_barriers.natural_causes == 1 | foodsecurity_access_barriers.no_funct_market == 1 |foodsecurity_access_barriers.inability_refusal_government_foodaid == 1 | foodsecurity_access_barriers.refusal_ngo_foodaid == 1 | foodsecurity_access_barriers.accessing_pay_portion_gatekeeper == 1 | foodsecurity_access_barriers.accessing_pay_portion_landlord == 1 ~ 3,
    foodsecurity_access_barriers.no_land_livestock == 1 | foodsecurity_access_barriers.econ_causes == 1 | foodsecurity_access_barriers.social_cultural_causes == 1 | foodsecurity_access_barriers.perception_beliefs == 1 ~ 2
  ),
  
  ## food security index3
  food_sec_index3 = case_when(
    foodsecurity_coping_food.gather_wild_food == 1 | foodsecurity_coping_food.gather_firewood == 1 | foodsecurity_coping_food.consume_seeds == 1 | foodsecurity_coping_food.displacement_camp == 1 | foodsecurity_coping_food.limit_meal_size == 1 | foodsecurity_coping_food.only_children_eat == 1 | foodsecurity_coping_food.reduce_meals == 1 | foodsecurity_coping_food.skip_days == 1 ~ 4,
    foodsecurity_coping_food.sell_home_assets == 1 | foodsecurity_coping_food.sell_livestock == 1 | foodsecurity_coping_food.slaughter_livestock == 1 | foodsecurity_coping_food.hunting == 1 | foodsecurity_coping_food.fishing == 1 ~ 3,
    foodsecurity_coping_food.borrow_food == 1 | foodsecurity_coping_food.household_begs == 1 | foodsecurity_coping_food.borrow_money == 1 | foodsecurity_coping_food.send_children_to_neighbors == 1 | foodsecurity_coping_food.less_expensive_food == 1 ~ 2
  ),
  
  #### Food Security Non-Critical Indicators  #### 
  ## food security nc index1
  food_sec_nc_index1 = case_when(
    foodsecurity_primary %in% c("food_assist_ngo", "food_assist_govt", "gifts_friends_family", "borrow_debt") ~ 1,
    foodsecurity_primary %in% c("market_purchases", "hh_production", "own_livestock", "fish_forage_hunt", "trade_labour") ~ 0
  ),
  
  ## food security nc index2
  food_sec_nc_index2 = case_when(
    male_small_scale_crop %in% c("none", "few") ~ 1,
    male_small_scale_crop %in% c("some", "many", "all") ~ 0
  ),
  
  ## food security nc index3
  food_sec_nc_index3 = case_when(
    female_small_scale_crop %in% c("none", "few") ~ 1,
    female_small_scale_crop %in% c("some", "many", "all") ~ 0
  ),
  
  ## food security nc index4
  food_sec_nc_index4 = case_when(
    male_animal_husband_livestock %in% c("none", "few") ~ 1,
    male_animal_husband_livestock %in% c("some", "many", "all") ~ 0
  ),
  
  ## food security nc index5
  food_sec_nc_index5 = case_when(
    female_animal_husband_livestock %in% c("none", "few") ~ 1,
    female_animal_husband_livestock %in% c("some", "many", "all") ~ 0
  ),
  
  ## food security nc index6
  food_sec_nc_index6 = case_when(
    fishing %in% c("none", "few") ~ 1,
    fishing %in% c("some", "many", "all") ~ 0
  ),
  
  ## food security nc index7
  food_sec_nc_index7 = case_when(
    foodsecurity_access == "no" ~ 1,
    foodsecurity_access == "yes" ~ 0
  ),
  
  ## food security nc index8
  food_sec_nc_index8 = case_when(
    foodsecurity_access_distance_min == "more_60" ~ 1,
    foodsecurity_access_distance_min %in% c("less_15", "1530", "3160") ~ 0
  ),
  
  ## food security nc index9
  food_sec_nc_index9 = case_when(
    support.cash == 1 | support.food_distrib == 1 ~ 0,
    support.cash == 0 | support.food_distrib == 0 ~ 1
  )
  
) %>%  mutate(
#### Additional indicators ####
  # additional_indicator_hlp = case_when(
  #   evictions_landowner %in% c("no_owner") & evictions_tenureagreement == "no" ~ 1,
  #   TRUE ~ 0
  #   ),


  # additional_indicator_minority = case_when(
  #   belonging_minority_group == "yes" & support_access_impediments.minorities == 1 ~ 1,
  #   TRUE ~0 
  # ),
  
  
  additional_indicator_access = case_when(
    localisation_district_label %in% c("Belet Weyne",
                                       "Jowhar",
                                       "Balcad",
                                       "Dharkenley",
                                       "Daynile",
                                       "Kahda",
                                       "Kismaayo",
                                       "Luuq",
                                       "Baardheere",
                                       "Doolow",
                                       "Afmadow",
                                       "Garbahaarey",
                                       "Ceel Waaq",
                                       "Belet Xaawo",
                                       "Bossaso",
                                       "Buuhoodle",
                                       "Xudun",
                                       "Laasqoray",
                                       "Marka",
                                       "Afgooye",
                                       "Wanla Weyn",
                                       "Baydhaba",
                                       "Diinsoor",
                                       "Qansax Dheere",
                                       "Xudur",
                                       "Ceel Barde",
                                       "Waajid") ~ 1,
    
    localisation_district_label %in% c("Gaalkacyo",
                                       "Galdogob",
                                       "Hobyo",
                                       "Dhuusamarreeb",
                                       "Cabudwaaq",
                                       "Cadaado",
                                       "Qardho",
                                       "Garoowe",
                                       "Burtinle",
                                       "Borama",
                                       "Baki",
                                       "Lughaye",
                                       "Hargeysa",
                                       "Burco",
                                       "Owdweyne",
                                       "Sheikh",
                                       "Laas Caanood",
                                       "Caynabo",
                                       "Ceerigaabo",
                                       "Ceel Afweyn") ~ 0
  )
) %>%  mutate(
  minorities_binary_1 = ifelse(nfi_access_impediments.minorities == 1 , 1 , 0),
  minorities_binary_2 = ifelse(sanitation_access_impediments.minorities == 1 , 1 , 0),
  minorities_binary_3 = ifelse(support_access_impediments.minorities == 1 , 1 , 0),
  minorities_binary_4 = ifelse(residents_no_food=="minority_headed_hh" , 1 , 0),
  minorities_binary_5 = ifelse(unwilling_make_complaint_feedback == "minority_clan_member" , 1 , 0),
  
  pwd_binary_1 = ifelse(sanitation_access_impediments.disabled == 1,1,0),
  pwd_binary_2 = ifelse(health_barriers.pwd_excluded == 1,1,0),
  pwd_binary_3 = ifelse(support_access_impediments.disabled == 1,1,0),
  pwd_binary_4 = ifelse(pwd_obstacles_complaints.fear_harassment == 1,1,0),
  pwd_binary_5 = ifelse(pwd_obstacles_complaints.lack_information_raise_complain == 1,1,0),
  pwd_binary_6 = ifelse(reason_nt_using_complaint_mechanisms.difficulty_pwd == 1,1,0)

  
) %>% mutate(
  
  additional_minorities_index = ifelse(rowSums(.[grep("minorities_binary_",names(.))], na.rm = TRUE)>0,1,0),
  additional_idp_index = ifelse(cccm_idps_arrived < 100 , 0 , 1),
  additional_pwd_index = ifelse(rowSums(.[grep("pwd_binary_",names(.))], na.rm = TRUE)>0,1,0),

  ) %>% select(-grep("_binary_",names(.)))
  

# data_indicators$additional_minorities_index %>% table()
# data_indicators$additional_idp_index %>% table()
# data_indicators$additional_pwd_index %>% table()

