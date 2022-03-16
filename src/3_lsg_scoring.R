source("src/functions/utils.R")

##### Computing LSGs ##### 

hlp_critical_indicators <- c("hlp_index1", "hlp_index2", "hlp_index3")
hlp_non_critical_indicators <- c("hlp_nc_index1", "hlp_nc_index2")

snfi_critical_indicators <- c("nfi_index1")
snfi_non_critical_indicators <- c("nfi_nc_index2", "nfi_nc_index3", "nfi_nc_index4", "nfi_nc_index5", "nfi_nc_index6")


wash_critical_indicators <- c("wash_index1", "wash_index2", "wash_index3", "wash_index4")
wash_non_critical_indicators <- c("wash_nc_index", "wash_nc_index2", "wash_nc_index3", "wash_nc_index4", "wash_nc_index5")


health_critical_indicators <- c("health_index1", "health_index2", "health_index3", "health_index4")
health_non_critical_indicators <- c("health_nc_index1", "health_nc_index2", "health_nc_index3", "health_nc_index4", "health_nc_index5", "health_nc_index6", "health_nc_index7", "health_nc_index8", "health_nc_index9", "health_nc_index10", "health_nc_index11", "health_nc_index12", "health_nc_index13")

                                  
nutrition_non_critical_indicators <- c("nutrition_nc_index1", "nutrition_nc_index2", "nutrition_nc_index3")


fs_critical_indicators <- c("food_sec_index2", "food_sec_index3")
fs_non_critical_indicators <- c("food_sec_nc_index1", "food_sec_nc_index2", "food_sec_nc_index3", "food_sec_nc_index4", "food_sec_nc_index5", "food_sec_nc_index6", "food_sec_nc_index7", "food_sec_nc_index8")



education_critical_indicators <- c("education_index1", "education_index2")
education_non_critical_indicators <- c("education_nc_index1", "education_nc_index2", "education_nc_index3", "education_nc_index4", "education_nc_index5", "education_nc_index6", "education_nc_index7", "education_nc_index8", "education_nc_index9")


protection_critical_indicators <- c("protection_index1", "protection_index2")
protection_non_critical_indicators <- c("protection_nc_index1", "protection_nc_index2", "protection_nc_index3", "protection_nc_index4", "protection_nc_index5")




data_indicators <-  data_indicators %>% 
  mutate(
    mean_nc_snfi = score_mean(rowMeans(select(.,!!!syms(snfi_non_critical_indicators)), na.rm = T)),
    mean_nc_wash = score_mean(rowMeans(select(.,!!!syms(wash_non_critical_indicators)), na.rm = T)),
    mean_nc_hlt = score_mean(rowMeans(select(.,!!!syms(health_non_critical_indicators)), na.rm = T)),
    mean_nc_nutrition = score_mean(rowMeans(select(.,!!!syms(nutrition_non_critical_indicators)), na.rm = T)),
    mean_nc_fs = score_mean(rowMeans(select(.,!!!syms(fs_non_critical_indicators)), na.rm = T)),
    mean_nc_edu = score_mean(rowMeans(select(.,!!!syms(education_non_critical_indicators)), na.rm = T)),
    mean_nc_protection = score_mean(rowMeans(select(.,!!!syms(protection_non_critical_indicators)), na.rm = T)),
    mean_nc_hlp = score_mean(rowMeans(select(.,!!!syms(hlp_non_critical_indicators)), na.rm = T))
    
  ) 


data_indicators <-  data_indicators %>% 
  
  mutate(
    
    snfi_score = pmax(!!!syms(snfi_critical_indicators),mean_nc_snfi,na.rm = T),
    
    wash_score = pmax(!!!syms(wash_critical_indicators),mean_nc_wash,na.rm = T),
    
    protection_score = pmax(!!!syms(protection_critical_indicators),mean_nc_protection,na.rm = T),
    
    fs_score = pmax(!!!syms(fs_critical_indicators),mean_nc_fs,na.rm = T),
    
    health_score = pmax(!!!syms(health_critical_indicators),mean_nc_hlt,na.rm = T),
    
    nutrition_score = mean_nc_nutrition,
    
    education_score = pmax(!!!syms(education_critical_indicators),mean_nc_edu,na.rm = T),
    
    hlp_score = pmax(!!!syms(hlp_critical_indicators),mean_nc_hlp,na.rm = T)
    
  ) 

data_indicators <- data_indicators %>% 
  
  mutate(
    
    protection_need = case_when(
      protection_score <= 2 ~ 0,
      protection_score == 3 ~ 1,
      protection_score > 3 ~ 2
      ),
    
    education_need = case_when(
      education_score <= 2 ~ 0,
      education_score == 3 ~ 1,
      education_score > 3 ~ 2),
    
    nutrition_need = case_when(
      nutrition_score <= 2 ~ 0,
      nutrition_score == 3 ~ 1,
      nutrition_score > 3 ~ 2),
    
    fs_need = case_when(
      fs_score <= 2 ~ 0,
      fs_score == 3 ~ 1,
      fs_score > 3 ~ 2),
    
    snfi_need = case_when(
      snfi_score <= 2 ~ 0,
      snfi_score == 3 ~ 1,
      snfi_score > 3 ~ 2),
    
    wash_need = case_when(
      wash_score <= 2 ~ 0,
      wash_score == 3 ~ 1,
      wash_score > 3 ~ 2),
    
    health_need = case_when(
      health_score <= 2 ~ 0,
      health_score == 3 ~ 1,
      health_score > 3 ~ 2),
    
    hlp_need = case_when(
      hlp_score <= 2 ~ 0,
      hlp_score == 3 ~ 1,
      hlp_score > 3 ~ 2)
  )


data_indicators <- data_indicators %>% 
  
  mutate(
    number_of_needs = rowSums(select(., protection_need, wash_need, health_need, education_need, fs_need, nutrition_need, snfi_need, hlp_need), na.rm=T)
  )

data_indicators <- data_indicators %>% mutate(
  site_duration_score = case_when(
    duration_site_established_in_months <= 24 ~ 1,
    TRUE ~ 0
  )
)



output1 <- data_indicators %>% select(
  "localisation_region_label",
  "localisation_district_label",
  "idp_code",
  "settlement_name",
  "site_duration_score",
  matches("index[[:digit:]]"),
  starts_with("additional_"),
  ends_with("_score"),
  ends_with("_need"),
  number_of_needs) 


output2 <- data_indicators %>% select(
  "localisation_region_label",
  "localisation_district_label",
  "idp_code",
  "settlement_name",
  "site_duration_score",
  starts_with("additional_"),
  ends_with("_need"),
  number_of_needs) 


openxlsx::write.xlsx(list(output2,output1),"output/Prioritization matrix/DSA_sites_matrix.xlsx")

write.csv(data_indicators,'output/Indicators/aggregation_output_plus_lsg.csv',row.names = F)


