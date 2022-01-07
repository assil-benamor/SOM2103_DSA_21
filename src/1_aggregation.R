rm(list = ls())

if (!require("pacman")) install.packages("pacman") 

p_load(rio,
       tidyverse,
       koboquest,
       crayon,
       hypegrammaR,
       composr)


source("./src/functions/aggregation_script_functions.R")


##### Loading dataset and preprocessing ##### 

## Loading the dataset and questionnaire
files.list <- list()

files.list$data <- "input/data/SOM2103_CleanData_2022-01-04.xlsx"
files.list$kobo <- "input/tool/REACH_SOM_DSA_Survey_Tool_v7.xlsx"

data <- import(files.list$data, sheet="Clean Data")  
questions <-  import(files.list$kobo,sheet="survey") %>% select(-1) %>% filter(!is.na(name))


## Deleting empty variables if any 
data <- mutate_if(data, is.character, na_if, "")  
data <- data %>% select_if(~ !(all(is.na(.x))))




## Colnames renaming
colnames(data) <- gsub("/",".",colnames(data))
colnames(data) <- gsub("^X_","",colnames(data))

## Making sure that numerical questions are parsed as numerical variables in the data /
## Same for select multiple choices 
listOfNumericQuestions <- questions %>% filter(type %in% c("integer","numeric","calculate")) %>% pull(name)

numericQuestions_index <- which(colnames(data) %in% listOfNumericQuestions)
data[numericQuestions_index] <- sapply(data[numericQuestions_index],as.numeric)

multipleSelectQuestions <- grep(pattern="\\.", colnames(data))
data[multipleSelectQuestions] <- sapply(data[multipleSelectQuestions],as.numeric)


# data [data == 999] <- NA
# data [data == ""] <- NA
# data [data == "dnk"] <- NA


site_leadership <- c("site_manager", "comm_leader", "local_govt_rep", "ngo_rep", "gatekeeper") # may be change 


## Cleaning the env
rm(list = c("listOfNumericQuestions",
            "multipleSelectQuestions",
            "numericQuestions_index"))




################### Data Aggregation ###################*
aggregation_columns <- "idp_code"
################### ################### ################*

##### Select multiple aggregation ##### 
select_multiple_questions <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_multiple") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  pull(name)


select_multiple <- data %>% 
  select(starts_with(paste0(select_multiple_questions,"."))) %>% colnames()


## Using max function to include all the selected choices for the differnet questions  
aggregation_output_select_multiple <- data %>% group_by(.dots =aggregation_columns) %>% 
  dplyr::summarize_at(.vars = select_multiple,.funs = max, na.rm=T)

aggregation_output_select_multiple[aggregation_output_select_multiple == -Inf] <- 0

aggregation_output_select_multiple <- select_multiple_apply_constraints(aggregation_output_select_multiple)

## Generating the character columns from the binaries
sm_list <- gsub("\\..*","",colnames(aggregation_output_select_multiple)[str_detect(colnames(aggregation_output_select_multiple),"\\.")]) %>% unique() 


aggregation_output_select_multiple <- cbind(generate_from_binaries(aggregation_output_select_multiple,sm_list),aggregation_output_select_multiple)


select_multiple_output <- aggregation_output_select_multiple %>% select(any_of(data %>% select(starts_with(sm_list)) %>% names())) 



## Cleaning the env
rm(list = c("aggregation_output_select_multiple",
  "select_multiple","sm_list"))

##### Select one aggregation ##### 

## Creating a list of questions for each aggregation type 

select_one_mode <- import("input/aggregation/master_list.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_one") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  filter(`For select one: Yes prevelance` == "no") %>% 
  pull(name)

select_one_mode <- c(select_one_mode,"nfi_access_dist_min_int","nfi_access_distance_max") ## Adding recoded questions

select_one_yes <- import("input/aggregation/master_list.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_one") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  filter(`For select one: Yes prevelance`== "yes") %>% 
  pull(name)


select_one_output <- merge(
data %>% group_by(.dots =aggregation_columns) %>% 
  dplyr::summarize_at(.vars = select_one_mode,.funs = fn_select_one_mode) ,

data %>% group_by(.dots =aggregation_columns) %>% 
  dplyr::summarize_at(.vars = select_one_yes,.funs = fn_select_one_yes_prevalence) 
)


## Cleaning the env
rm(list = c("select_one_mode",
            "select_one_yes"))



##### Numerical values aggregation ##### 

numerical_questions_mode_all <- import("input/aggregation/master_list.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="Numerical") %>% 
  filter(`For Numerical: Recode to categorical?`=="no") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  pull(name)

numerical_questions_mode_subset <- import("input/aggregation/master_list.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="Numerical") %>% 
  filter(`For Numerical: Recode to categorical?`=="no") %>% 
  filter(`Aggregation : all / subset`== "subset") %>% 
  pull(name)


numerical_values_output_all <- data %>% group_by(.dots =aggregation_columns) %>% 
  dplyr::summarize_at(.vars = numerical_questions_mode_all,.funs = one_sd_mean) 

numerical_values_output_subset <- data %>% group_by(.dots =aggregation_columns) %>% 
  dplyr::summarize_at(.vars = numerical_questions_mode_subset,.funs = one_sd_mean_subset) 


numerical_values_output <- merge(
  numerical_values_output_all,
  numerical_values_output_subset
)

#### Recoding cccm_site_duration question
numerical_values_output <- numerical_values_output %>%  new_recoding(target = cccm_site_duration_rec) %>%
  recode_to(to = "less_than_6_months", where = cccm_site_duration <6 ) %>%
  recode_to(to = "6_months_1_year", where = cccm_site_duration >=6 & cccm_site_duration <12 ) %>%
  recode_to(to = "1_year_2_years", where = cccm_site_duration >=12 & cccm_site_duration <24 ) %>%
  recode_to(to = "2_year_5_years", where = cccm_site_duration >=24 & cccm_site_duration <60 ) %>%
  recode_to(to = "more_than_5_years", where = cccm_site_duration >=60 ) %>% 
end_recoding()  


numerical_values_output$cccm_site_duration <- numerical_values_output$cccm_site_duration_rec
numerical_values_output$cccm_site_duration_rec <- NULL


rm(list = c("numerical_questions_mode_all", 
            "numerical_questions_mode_subset",
            "numerical_values_output_all",
            "numerical_values_output_subset"))




######### Merge outputs ######### 

all_questions_output <- Reduce(function(...) merge(..., all=TRUE), list(select_one_output, 
                                                select_multiple_output,
                                                numerical_values_output))

all_questions_output <- subset(all_questions_output, select=lapply(colnames(data), match,table=colnames(all_questions_output)) %>% 
                                 unlist() %>%
                                 .[!is.na(.)] )

all_questions_output <- all_questions_output %>% select(3,4,1,2,5:ncol(all_questions_output))


all_questions_output[is.nan(all_questions_output)] <- NA

######### Skip logic ######### 
all_questions_output %>% mutate(across(where(is.factor), as.character)) -> all_questions_output

all_questions_output <- all_questions_output %>% 
  mutate(
    
    #### Skip logic code in tool : 58
    cccm_idps_arrived_last_month = replace(cccm_idps_arrived_last_month, 
                                           cccm_idps_arrived == 0, NA),
    #### Skip logic code in tool : 68
    cccm_idps_origin_second = replace(cccm_idps_origin_second, 
                                      cccm_idps_origin_first == "none", NA),
    
    #### Skip logic code in tool : 69
    cccm_district_origin_second = replace(cccm_district_origin_second, 
                                          cccm_idps_origin_first == "none", NA),
    
    #### Skip logic code in tool : 86
    evictions_tenureagreement = replace(evictions_tenureagreement, 
                                        evictions_landowner %in% c("no_owner","dnk","pnta"),
                                        NA),
    
    #### Skip logic code in tool : 139
    nfi_access_distance_max = replace(nfi_access_distance_max, 
                                      nfi_access_distance_min == "dnk", NA),
    
    
    #### Skip logic code in tool : 146
    water_sources_functional = replace(water_sources_functional, 
                                       water_sources_present == 0, NA),
    #### Skip logic code in tool : 160
    water_treatment_proportion = replace(water_treatment_proportion, 
                                         water_treatment_methods %in% c("no_treatment",
                                                                        "dnk",
                                                                        "no_treatment dnk"),
                                         NA),
    #### Skip logic code in tool : 162
    water_access_distance_min = replace(water_access_distance_min, 
                                        water_sources_present == 0, NA),
    
    #### Skip logic code in tool : 165
    water_access_barriers = replace(water_access_barriers, 
                                    water_sources_present == 0, NA),
    
    #### Skip logic code in tool : 175
    hygiene_handwashingfacilities = replace(hygiene_handwashingfacilities, 
                                            sanitation_toilets_total == 0, NA),
    sanitation_lockabletoilets = replace(sanitation_lockabletoilets, 
                                         sanitation_toilets_total == 0, NA),
    sanitation_toiletlighting = replace(sanitation_toiletlighting, 
                                        sanitation_toilets_total == 0, NA),
    sanitation_toilet_access_pwd = replace(sanitation_toilet_access_pwd, 
                                           sanitation_toilets_total == 0, NA),
    
    #### Skip logic code in tool : 181
    sanitation_desludging = replace(sanitation_desludging, 
                                    sanitation_toilets_total == 0, NA),
    
    #### Skip logic code in tool : 187
    sanitation_access_distance_min = replace(sanitation_access_distance_min, 
                                             sanitation_toilets_total == 0, NA),
    
    #### Skip logic code in tool : 189
    sanitation_access_impediments = replace(sanitation_access_impediments, 
                                            sanitation_toilets_total == 0, NA),
    
    #### Skip logic code in tool : 191
    hygiene_access_distance_min = replace(hygiene_access_distance_min, 
                                          hygiene_bathingfacilities == 0, NA),
    
    #### Skip logic code in tool : 191
    latrine_use = replace(latrine_use, 
                          hygiene_bathingfacilities == 0, NA),
    
    #### Skip logic code in tool : 191
    hygiene_access_impediments = replace(hygiene_access_impediments, 
                                         hygiene_bathingfacilities == 0, NA),
    
    #### Skip logic code in tool : 221
    health_access_distance_min = replace(health_access_distance_min, 
                                         health_facilities == "no_health_facility", NA),
    
    #### Skip logic code in tool : 224
    health_barriers = replace(health_barriers, 
                              health_facilities == "no_health_facility", NA),
    
    #### Skip logic code in tool : 247
    fully = replace(fully,education_facilities %in% c("no_available"), NA),
    partially = replace(partially,education_facilities %in% c("no_available"), NA),
    not_functional = replace(not_functional,education_facilities %in% c("no_available"), NA),
    
    #### Skip logic code in tool : 256
    education_access_distance_min = replace(education_access_distance_min,
                                            education_facilities %in% c("no_available"), NA),
    
    #### Skip logic code in tool : 280
    foodsecurity_access_barriers = replace(foodsecurity_access_barriers,
                                           foodsecurity_livelihood == "none", NA),
    
    #### Skip logic code in tool : 282
    foodsecurity_coping_food = replace(foodsecurity_coping_food,
                                       foodsecurity_livelihood == "none", NA),
    
    #### Skip logic code in tool : 284
    other_reason_coping = replace(other_reason_coping,
                                  foodsecurity_livelihood == "none", NA),
    
    
    #### Skip logic code in tool : 286
    hunger_level = replace(hunger_level,foodsecurity_livelihood == "none", NA),
    
    #### Skip logic code in tool : 290
    foodsecurity_access_distance_min = replace(foodsecurity_access_distance_min,
                                               foodsecurity_access == "no", NA),
    
    #### Skip logic code in tool : 291
    foodsecurity_access_distance_max = replace(foodsecurity_access_distance_max,
                                               foodsecurity_access == "no", NA),
    
    #### Skip logic code in tool : 304
    protection_incidents_place = replace(protection_incidents_place,
                                         protection_incidents == "no_incidents", NA),
    
    #### Skip logic code in tool : 319
    support_access_impediments = replace(support_access_impediments,
                                         support %in% c("none","dnk","none dnk"), NA),
    
    #### Skip logic code in tool : 343
    cccm_cfm = replace(cccm_cfm,aap_feedbackmechanism != "yes", NA),
    
    #### Skip logic code in tool : 344
    aap_access_barriers = replace(aap_access_barriers,
                                  aap_feedbackmechanism != "yes", NA),
    
  )



all_questions_output <- all_questions_output %>% mutate(
  sanitation_access_impediments.women = ifelse(is.na(sanitation_access_impediments),NA,sanitation_access_impediments.women),
  sanitation_access_impediments.children = ifelse(is.na(sanitation_access_impediments),NA,sanitation_access_impediments.children),
  sanitation_access_impediments.elders = ifelse(is.na(sanitation_access_impediments),NA,sanitation_access_impediments.elders),
  sanitation_access_impediments.disabled = ifelse(is.na(sanitation_access_impediments),NA,sanitation_access_impediments.disabled),
  sanitation_access_impediments.minorities = ifelse(is.na(sanitation_access_impediments),NA,sanitation_access_impediments.minorities),
  sanitation_access_impediments.no_impediments = ifelse(is.na(sanitation_access_impediments),NA,sanitation_access_impediments.no_impediments),
  sanitation_access_impediments.dnk = ifelse(is.na(sanitation_access_impediments),NA,sanitation_access_impediments.dnk),
  sanitation_access_impediments.other = ifelse(is.na(sanitation_access_impediments),NA,sanitation_access_impediments.other),
  health_barriers.no_problem = ifelse(is.na(health_barriers),NA,health_barriers.no_problem),
  health_barriers.cost = ifelse(is.na(health_barriers),NA,health_barriers.cost),
  health_barriers.no_qualified = ifelse(is.na(health_barriers),NA,health_barriers.no_qualified),
  health_barriers.documents = ifelse(is.na(health_barriers),NA,health_barriers.documents),
  health_barriers.no_referral = ifelse(is.na(health_barriers),NA,health_barriers.no_referral),
  health_barriers.not_open = ifelse(is.na(health_barriers),NA,health_barriers.not_open),
  health_barriers.far_away = ifelse(is.na(health_barriers),NA,health_barriers.far_away),
  health_barriers.refuse_treatment = ifelse(is.na(health_barriers),NA,health_barriers.refuse_treatment),
  health_barriers.no_medicine = ifelse(is.na(health_barriers),NA,health_barriers.no_medicine),
  health_barriers.no_treatment_avail = ifelse(is.na(health_barriers),NA,health_barriers.no_treatment_avail),
  health_barriers.pwd_excluded = ifelse(is.na(health_barriers),NA,health_barriers.pwd_excluded),
  health_barriers.dnk = ifelse(is.na(health_barriers),NA,health_barriers.dnk),
  health_barriers.other = ifelse(is.na(health_barriers),NA,health_barriers.other),
  foodsecurity_access_barriers.sec_issues = ifelse(is.na(foodsecurity_access_barriers),NA,foodsecurity_access_barriers.sec_issues),
  foodsecurity_access_barriers.no_land_livestock = ifelse(is.na(foodsecurity_access_barriers),NA,foodsecurity_access_barriers.no_land_livestock),
  foodsecurity_access_barriers.natural_causes = ifelse(is.na(foodsecurity_access_barriers),NA,foodsecurity_access_barriers.natural_causes),
  foodsecurity_access_barriers.econ_causes = ifelse(is.na(foodsecurity_access_barriers),NA,foodsecurity_access_barriers.econ_causes),
  foodsecurity_access_barriers.no_funct_market = ifelse(is.na(foodsecurity_access_barriers),NA,foodsecurity_access_barriers.no_funct_market),
  foodsecurity_access_barriers.other = ifelse(is.na(foodsecurity_access_barriers),NA,foodsecurity_access_barriers.other),
  foodsecurity_access_barriers.dnk = ifelse(is.na(foodsecurity_access_barriers),NA,foodsecurity_access_barriers.dnk),
  foodsecurity_access_barriers.pnta = ifelse(is.na(foodsecurity_access_barriers),NA,foodsecurity_access_barriers.pnta),
  protection_incidents_place.in_shelters = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.in_shelters),
  protection_incidents_place.leaving_settlement = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.leaving_settlement),
  protection_incidents_place.way_to_nfi = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.way_to_nfi),
  protection_incidents_place.way_to_food = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.way_to_food),
  protection_incidents_place.way_to_water = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.way_to_water),
  protection_incidents_place.way_to_latrines = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.way_to_latrines),
  protection_incidents_place.way_to_bathing = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.way_to_bathing),
  protection_incidents_place.way_to_school = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.way_to_school),
  protection_incidents_place.way_to_hc = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.way_to_hc),
  protection_incidents_place.at_aid_distrib = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.at_aid_distrib),
  protection_incidents_place.dnk = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.dnk),
  protection_incidents_place.pnta = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.pnta),
  protection_incidents_place.other = ifelse(is.na(protection_incidents_place),NA,protection_incidents_place.other)
)


#### Adding settelments names to the aggregated data
 settlement_names <- data %>% select(idp_code,localisation_settlement_name) %>% 
  mutate(localisation_settlement_name = tools::toTitleCase(tolower(str_squish(localisation_settlement_name)))) %>% 
  unique() %>% 
  group_by(idp_code) %>% 
   summarise(
     localisation_settlement_name = paste(localisation_settlement_name,collapse = " / ")
   )
 
 
 all_questions_output <- merge(all_questions_output,settlement_names)%>% select(1,ncol(all_questions_output)+1,2:ncol(all_questions_output))
 all_questions_output[all_questions_output == ""] <- NA

# survey_data$type <- "0_Original_survey"
# all_questions_output$type <- "1_Aggregated_survey"
# 
# survey_data <- survey_data %>% select(1,ncol(.),2:ncol(.)-1) 
# all_questions_output <- all_questions_output %>% select(1,ncol(.),2:ncol(.)-1) 
# 
# combined_output <- rbind(survey_data,as.data.frame(all_questions_output))
# 
# 
# write.csv(survey_data,"output/survey_data.csv",
#            na = "NA",row.names = F)
 
#### Fix NC in localisation_district and localisation_region
 
 all_questions_output <- all_questions_output %>% 
   mutate(
     
     localisation_district = replace(localisation_district, 
                                     idp_code %in%  c("DSA4_SO1301_004",
                                                      "DSA4_SO1301_005"), "burco"),
     
     localisation_district = replace(localisation_district, 
                                     idp_code == "DSA4-LJ-01", "marka"),
     
     localisation_region = replace(localisation_region, 
                                     idp_code == "DSA4-LJ-01", "lower_shabelle"),
     
     localisation_district = replace(localisation_district, 
                                     idp_code == "DSA4-SO12-002", "berbera"),
     
     localisation_district = replace(localisation_district, 
                                     idp_code == "DSA4-SO24-140", "baidoa"),
     
     localisation_district = replace(localisation_district, 
                                     idp_code == "DSA4-SO24-221", "diinsoor"),
   )


write.csv(all_questions_output,"output/Aggregation/aggregation_output1306.csv",
          na = "NA",row.names = F)






