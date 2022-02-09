## Additional Indicators
rm(list = ls())

# load packages
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

# create additional indicator index
additional_indicators <- data %>% mutate(
  # hlp_index1
  hlp_index1 = case_when(
    evictions_landowner %in% c("no_owner") ~ 1,
    evictions_landowner %in% c("fed_govt", "local_authority_govt", "mixed", "private_owner") ~ 0
  ),
  
  # hlp_index2
  hlo_index2 = case_when(
    evictions_tenureagreement == "no" ~ 1,
    evictions_tenureagreement == "yes" ~ 0
  ),
  
  # minority_index1
  minority_index1 = case_when(
    belonging_minority_group == "yes" ~ 1,
    belonging_minority_group == "no" ~ 0
  ),
  
  # minority_index2
  minority_index2 = case_when(
    support_access_impediments.minorities == 1 ~ 1,
    
    support_access_impediments.women == 1 | support_access_impediments.children == 1 | support_access_impediments.elders == 1 | support_access_impediments.disabled == 1 | support_access_impediments.marginalised == 1 ~ 0                                 
  ), 
  
  # Access
  overall = case_when(
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
                                       "Waajid") ~ "B",
    
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
                                       "Ceel Afweyn") ~ "C"
  )
  
  
)#

