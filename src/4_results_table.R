rm(list = ls())

options(scipen = 999)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               hypegrammaR,
               rio,
               readxl,
               openxlsx)


source("src/functions/utils.R")
source("src/functions/results_table_functions_weigths_improved_purrr.R")

data <- read.csv("output/Indicators/aggregation_output_plus_lsg.csv",
                 stringsAsFactors = F,
                 na.strings = c("NA","#N/A",""," "))


koboToolPath = "input/tool/REACH_SOM_DSA_Survey_Tool_for_results_table.xlsx"

questions <- read_xlsx(koboToolPath,
                       guess_max = 50000,
                       na = c("NA","#N/A",""," "),
                       sheet = 1) %>% filter(!is.na(name)) %>% 
  mutate(q.type=as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
         list_name=as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
         list_name=ifelse(str_starts(type, "select_"), list_name, NA))
  


choices <- read_xlsx(koboToolPath,
                       guess_max = 50000,
                       na = c("NA","#N/A",""," "),
                       sheet = 2)


                           
data <- data %>% mutate_at(c("wash_index1", "wash_index2", "wash_index3", "wash_index4", "wash_nc_index", "wash_nc_index2", "wash_nc_index3", "wash_nc_index4", "wash_nc_index5", "protection_index1", "protection_index2", "protection_nc_index1", "protection_nc_index2", "protection_nc_index3", "protection_nc_index4", "protection_nc_index5", "education_nc_index1", "education_nc_index2", "education_nc_index3", "education_nc_index4", "education_nc_index5", "education_nc_index6", "education_nc_index7", "education_nc_index8", "education_nc_index9", "education_index1", "education_index2", "health_index1", "health_index2", "health_index3", "health_index4", "health_nc_index1", "health_nc_index2", "health_nc_index3", "health_nc_index4", "health_nc_index5", "health_nc_index6", "health_nc_index7", "health_nc_index8", "health_nc_index9", "health_nc_index10", "health_nc_index11", "health_nc_index12", "health_nc_index13", "nutrition_nc_index1", "nutrition_nc_index2", "nutrition_nc_index3", "nfi_index1", "nfi_nc_index1", "nfi_nc_index2", "nfi_nc_index3", "nfi_nc_index4", "nfi_nc_index5", "nfi_nc_index6", "hlp_index1", "hlp_index2", "hlp_index3", "hlp_nc_index1", "hlp_nc_index2", "food_sec_index1", "food_sec_index2", "food_sec_index3", "food_sec_nc_index1", "food_sec_nc_index2", "food_sec_nc_index3", "food_sec_nc_index4", "food_sec_nc_index5", "food_sec_nc_index6", "food_sec_nc_index7", "food_sec_nc_index8", "hlp_additional_index1", "hlp_additional_index2", "minority_additional_index1", "minority_additional_index2", "humanitarian_access_additional_indicator", "mean_nc_snfi", "mean_nc_wash", "mean_nc_hlt", "mean_nc_nutrition", "mean_nc_fs", "mean_nc_edu", "mean_nc_protection", "mean_nc_hlp", "snfi_score", "wash_score", "protection_score", "fs_score", "health_score", "nutrition_score", "education_score", "hlp_score", "protection_need", "education_need", "nutrition_need", "fs_need", "snfi_need", "wash_need", "health_need", "hlp_need", "number_of_needs", "site_duration_score")
                           ,as.character)



data <- data %>% setnames(old = c("buul", "solid_apartment", "unfinished", "make_shift", "none"), 
                  new = c("hh_living_in_buul", "hh_living_in_solid_apartment", "hh_living_in_unfinished", "hh_living_in_make_shift", "hh_living_in_none"))



data <- data %>% select(localisation_region_label,c(settlement_type:site_duration_score))


res <- generate_results_table(data = data,
                              questions = questions,
                              choices = choices,
                              weights.column = NULL,
                              use_labels = T,
                              labels_column = "label::English (en)",
                              "localisation_region_label"

)

# is.nan.data.frame <- function(x)
#   do.call(cbind, lapply(x, is.nan))
# 
# res[is.nan(res)] <- NA


export_table(res,"output/Results table/")




