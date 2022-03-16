rm(list = ls())

if (!require("pacman")) install.packages("pacman") 


p_load(rio,
       tidyverse,
       koboquest,
       crayon,
       readxl,
       composr)


source("./src/functions/aggregation_script_functions.R")


##### Loading dataset and pre-processing ##### 

## Loading the dataset and questionnaire
files.list <- list()

files.list$data <- "input/data/SOM2103_CleanData_v3.xlsx"
files.list$kobo <- "input/tool/REACH_SOM_DSA_Survey_Tool_v7.xlsx"


data <- read_xlsx(files.list$data,
                  guess_max = 50000,
                  na = c("NA","#N/A",""," "),
                  sheet = "Clean Data"
)

questions <-  import(files.list$kobo,sheet="survey") %>% select(-1) %>% filter(!is.na(name))


## Deleting empty variables if any 

data <- data %>% select_if(~ !(all(is.na(.x))))

names(data)[!names(data) %in% names(data %>% select_if(~ !(all(is.na(.x)))))]

## Replacing / par . and removing leading ^_ in variables names
colnames(data) <- gsub("/",".",colnames(data))
colnames(data) <- gsub("^X_","",colnames(data))

## Making sure that numerical questions are parsed as numerical variables in the data 

## Same for select multiple choices 
listOfNumericQuestions <- questions %>% filter(type %in% c("integer","numeric","calculate")) %>% pull(name)

listOfNumericQuestions <- listOfNumericQuestions[!listOfNumericQuestions %in% c("district", "localisation_region_label", "localisation_district_label", "idp_code_verification", "idp_code_districti_verification")]

numericQuestions_index <- which(colnames(data) %in% listOfNumericQuestions)

data[numericQuestions_index] <- sapply(data[numericQuestions_index],as.numeric)

multipleSelectQuestions <- grep(pattern="\\.", colnames(data))

data[multipleSelectQuestions] <- sapply(data[multipleSelectQuestions],as.numeric)


## Recoding some typos in the data 
data$water_treatment_proportion <- str_to_lower(data$water_treatment_proportion ) 


# data [data == 999] <- NA
data [data == "dnk"] <- NA
data [data == "Yes"] <- "yes"



## Cleaning the env
rm(list = c("listOfNumericQuestions",
            "multipleSelectQuestions",
            "numericQuestions_index"))




################### Data Aggregation ###################*
aggregation_column <- "idp_code"
################### ################### ################*


##### Select multiple aggregation ##### 
select_multiple_questions <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_multiple") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  pull(name)


select_multiple <- data %>% 
  select(starts_with(paste0(select_multiple_questions,"."))) %>% colnames()


## Using max function to include all the selected choices for the different questions  
aggregation_output_select_multiple <- data %>% group_by(.dots =aggregation_column) %>% 
  dplyr::summarize_at(.vars = select_multiple,.funs = max, na.rm=T)


aggregation_output_select_multiple[aggregation_output_select_multiple == -Inf] <- as.numeric(NA) # Recode -Inf to NA

aggregation_output_select_multiple <- select_multiple_apply_constraints(aggregation_output_select_multiple)

## Generating the character columns from the binaries
sm_list <- gsub("\\..*","",colnames(aggregation_output_select_multiple)[str_detect(colnames(aggregation_output_select_multiple),"\\.")]) %>% unique() 


aggregation_output_select_multiple <- cbind(generate_from_binaries(aggregation_output_select_multiple,sm_list),aggregation_output_select_multiple)


select_multiple_output <- aggregation_output_select_multiple %>% select("idp_code", any_of(data %>% select(starts_with(sm_list)) %>% names())) 


## Cleaning the env
rm(list = c("aggregation_output_select_multiple",
  "select_multiple","sm_list","select_multiple_questions"))

##### Select one aggregation ##### 

###### Type1: No subset, No NC correction ######

select_one_outputs <- list()

select_one_type1 <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_one") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  filter(is.na(`If no subset : correct for Non Consensus (NC)?`)) %>% 
  filter(is.na(`For select one: Yes prevelance`)) %>% 
  filter(is.na(`For select one: No prevelance`)) %>%
  pull(name)


select_one_outputs$type1 <- data %>% group_by(.dots = aggregation_column) %>% 
  dplyr::summarize_at(.vars = select_one_type1,
                      .funs = fn_select_one_mode) 

#######  Type2: No subset, NC correction ###### 

######  2.1: NC correction using community leader/camp manager/gate keeper 

select_one_type2_1 <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_one") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  filter(`If no subset : correct for Non Consensus (NC)?` == "yes") %>% 
  filter(`If correct NC: roles?`== "community leader/camp manager/gate keeper") %>% 
  filter(is.na(`For select one: Yes prevelance`)) %>% 
  filter(is.na(`For select one: No prevelance`)) %>%
  pull(name)


select_one_outputs$type2_1 <- data %>% group_by(.dots = aggregation_column) %>%
  do(dplyr::summarize_at(.,.vars = select_one_type2_1,
                      .funs = fn_select_one_mode_nc_correction,
                      subset_var = .$"ki_role",
                      role = c("comm_leader", "site_manager", "gatekeeper")))



######  2.2: NC correction using pwd rep 

select_one_type2_2 <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_one") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  filter(`If no subset : correct for Non Consensus (NC)?` == "yes") %>% 
  filter(`If correct NC: roles?`== "pwd rep") %>% 
  filter(is.na(`For select one: Yes prevelance`)) %>% 
  filter(is.na(`For select one: No prevelance`)) %>%
  pull(name)

select_one_outputs$type2_2 <- data %>% group_by(.dots = aggregation_column) %>% 
  do(dplyr::summarize_at(.,.vars = select_one_type2_2,
                      .funs = fn_select_one_mode_nc_correction,
                      subset_var = .$"ki_role",
                      role = c("resident_site_pwd_rep")))


###### 2.3: NC correction using woman rep

select_one_type2_3 <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_one") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  filter(`If no subset : correct for Non Consensus (NC)?` == "yes") %>% 
  filter(`If correct NC: roles?`== "woman rep") %>% 
  filter(is.na(`For select one: Yes prevelance`)) %>% 
  filter(is.na(`For select one: No prevelance`)) %>%
  pull(name)

select_one_outputs$type2_3 <- data %>% group_by(.dots = aggregation_column) %>% 
  do(dplyr::summarize_at(.,.vars = select_one_type2_3,
                      .funs = fn_select_one_mode_nc_correction,
                      subset_var = .$"ki_role",
                      role = c("women_comm_rep")))




######  Type3: subset (community leader/camp manager/gate keeper) ###### 

select_one_type3 <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_one") %>% 
  filter(`Aggregation : all / subset`== "subset") %>% 
  filter(`If subset : roles ?`== "community leader/camp manager/gate keeper") %>% 
  filter(is.na(`For select one: Yes prevelance`)) %>% 
  filter(is.na(`For select one: No prevelance`)) %>%
  pull(name)


select_one_outputs$type3 <- data %>% 
  group_by(.dots = aggregation_column) %>% 
  do(dplyr::summarize_at(.,.vars = select_one_type3,
                      .funs = fn_select_one_mode_subset,
                      subset_var = .$"ki_role",
                      role = c("comm_leader", "site_manager", "gatekeeper")))


######  Type4: subset (women rep) ######  

select_one_type4 <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_one") %>% 
  filter(`Aggregation : all / subset`== "subset") %>% 
  filter(`If subset : roles ?`== "women rep") %>% 
  filter(is.na(`For select one: Yes prevelance`)) %>% 
  filter(is.na(`For select one: No prevelance`)) %>%
  pull(name)


select_one_outputs$type4 <- data %>% 
  group_by(.dots = aggregation_column) %>% 
  do(dplyr::summarize_at(.,.vars = select_one_type4,
                      .funs = fn_select_one_mode_subset,
                      subset_var = .$"ki_role",
                      role = c("women_comm_rep")))

######  Type5: subset (women rep/pwd rep ######  

select_one_type5 <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_one") %>% 
  filter(`Aggregation : all / subset`== "subset") %>% 
  filter(`If subset : roles ?`== "women rep/pwd rep") %>% 
  filter(is.na(`For select one: Yes prevelance`)) %>% 
  filter(is.na(`For select one: No prevelance`)) %>%
  pull(name)

select_one_outputs$type5 <- data %>% 
  group_by(.dots = aggregation_column) %>% 
  do(dplyr::summarize_at(.,.vars = select_one_type5,
                      .funs = fn_select_one_mode_subset,
                      subset_var = .$"ki_role",
                      role = c("women_comm_rep","resident_site_pwd_rep")))

######  Type6: No subset, Yes prevalence ######  

select_one_type6 <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_one") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  filter(!is.na(`For select one: Yes prevelance`)) %>% 
  filter(is.na(`If no subset : correct for Non Consensus (NC)?`)) %>% 
  pull(name)


select_one_outputs$type6 <- data %>% group_by(.dots = aggregation_column) %>% 
  dplyr::summarize_at(.vars = select_one_type6,.funs = fn_select_one_yes_prevalence) 



######  Type8: No subset, No prevalence ######  

select_one_type8 <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="select_one") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  filter(!is.na(`For select one: No prevelance`)) %>% 
  pull(name)


select_one_outputs$type8 <- data %>% group_by(.dots = aggregation_column) %>% 
  dplyr::summarize_at(.vars = select_one_type8,
                      .funs = fn_select_one_no_prevalence) 



select_one_output <- Reduce(function(...) merge(..., all=TRUE, by='idp_code'), select_one_outputs) %>% select(any_of(names(data)))


## Cleaning the env
rm(list = c("select_one_outputs",
            ls()[grep("^select_one_type",ls())]))




##### Numerical values aggregation ##### 

numerical_questions_mode_all <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="Numerical") %>% 
  filter(`Aggregation : all / subset`== "all") %>% 
  pull(name)

numerical_questions_mode_subset <- import("input/aggregation/Data aggregation plan_IDP Site level.xlsx") %>% 
  filter(`Include question`=="yes") %>% 
  filter(type=="Numerical") %>% 
  filter(`Aggregation : all / subset`== "subset") %>% 
  pull(name)


numerical_values_output_all <- data %>% group_by(.dots =aggregation_column) %>% 
  dplyr::summarize_at(.vars = numerical_questions_mode_all,.funs = one_sd_mean) 

numerical_values_output_subset <- data %>% group_by(.dots =aggregation_column) %>% 
  do(dplyr::summarize_at(.,.vars = numerical_questions_mode_subset,
                      .funs = one_sd_mean_subset,
                      subset_var = .$"ki_role",
                      role = c("women_comm_rep")))


numerical_values_output <- merge(
  numerical_values_output_all,
  numerical_values_output_subset
)


rm(list = c("numerical_questions_mode_all", 
            "numerical_questions_mode_subset",
            "numerical_values_output_all",
            "numerical_values_output_subset"))




######### Merge outputs ######### 

all_questions_output <- Reduce(function(...) merge(..., all=TRUE, by ='idp_code'), list(select_one_output, 
                                                select_multiple_output,
                                                numerical_values_output))

all_questions_output <- subset(all_questions_output, select=lapply(colnames(data), match,table=colnames(all_questions_output)) %>% 
                                 unlist() %>%
                                 .[!is.na(.)] )


all_questions_output <-  left_join(data %>% select(localisation_region_label,
                                                   localisation_region,
                                                   localisation_district_label,
                                                   district,
                                                   idp_code) %>% unique(),
                                    all_questions_output, by ="idp_code")




all_questions_output[is.nan(all_questions_output)] <- NA

rm(list = c("select_one_output", 
            "select_multiple_output",
            "numerical_values_output"))



######### Skip logic ######### 

SL_depend<-read.csv("input/Skip Logic/DSA_SL.csv", stringsAsFactors = FALSE, dec=".", sep=",", na.strings=c("NA",""," ")) %>% select(-1)            #import with blanks being NA's
inversed_critera <- SL_depend %>% pull(1)
SL_depend <- SL_depend %>% select(-1)
SL_depend <- SL_depend[!is.na(SL_depend[,1]),]  


for (i in 1:dim(SL_depend)[1]){
  
  dependent_questions_rgx <- sprintf("(%s)",map_chr(SL_depend[i,3:dim(SL_depend)[2]] %>%
                                                      t() %>% as.character() %>%
                                                      .[!is.na(.)], ~sprintf("^%s\\.|^%s$",.x,.x)) 
                                     %>% paste(collapse = "|"))
  
  index_SL_depend <- which(grepl(dependent_questions_rgx,names(all_questions_output)))
  

  if(inversed_critera[i]){
    index_SL_rows<- which(as.character(all_questions_output[[SL_depend[i,1]]]) == as.character(SL_depend[i,2]) )
  } else {
    index_SL_rows<- which(as.character(all_questions_output[[SL_depend[i,1]]]) != as.character(SL_depend[i,2]) )
  }
 
  
  all_questions_output[index_SL_rows,index_SL_depend] <- NA
  
}


all_questions_output <- all_questions_output %>% 
  
  mutate(
    
    #### unnamed_group_110 : not(selected(${evictions_landowner},"no_owner")) and not(selected(${evictions_landowner},"dnk")) and not(selected(${evictions_landowner},"pnta"))

    evictions_tenureagreement = replace(evictions_tenureagreement, 
                                        evictions_landowner %in% c("no_owner","evictions_landowner","pnta"), NA),
    
    evictions_tenureagreement_renewal = replace(evictions_tenureagreement_renewal, 
                                        evictions_landowner %in% c("no_owner","evictions_landowner","pnta"), NA),
    
    evictions_tenureagreement_rentpayment = replace(evictions_tenureagreement_rentpayment, 
                                        evictions_landowner %in% c("no_owner","evictions_landowner","pnta"), NA),
    
  
    #### not(selected(${evictions_tenureagreement_rentpayment},"no_rent_paid")) and not(selected(${evictions_tenureagreement_rentpayment},"dnk"))
   
    evictions_tenureagreement_rentfrequency = replace(evictions_tenureagreement_rentfrequency, 
                                                      evictions_tenureagreement_rentpayment %in% c("no_rent_paid","dnk"), NA),
    
    
    #### not(selected(${support} , "none")) and not(selected(${support}, "dnk"))
    
    support_access_impediments = replace(support_access_impediments, 
                                         support %in% c("none","dnk"), NA),
    
    support_access_impediments.women = replace(support_access_impediments.women, 
                                         support %in% c("none","dnk"), NA),
    
    support_access_impediments.children = replace(support_access_impediments.children, 
                                         support %in% c("none","dnk"), NA),
    
    support_access_impediments.elders = replace(support_access_impediments.elders, 
                                         support %in% c("none","dnk"), NA),
    
    support_access_impediments.disabled = replace(support_access_impediments.disabled, 
                                         support %in% c("none","dnk"), NA),
    
    support_access_impediments.minorities = replace(support_access_impediments.minorities, 
                                         support %in% c("none","dnk"), NA),
    
    support_access_impediments.marginalised = replace(support_access_impediments.marginalised, 
                                         support %in% c("none","dnk"), NA),
    
    support_access_impediments.no_impediments = replace(support_access_impediments.no_impediments, 
                                         support %in% c("none","dnk"), NA),
    
    support_access_impediments.dnk = replace(support_access_impediments.dnk, 
                                                        support %in% c("none","dnk"), NA),
    
    support_access_impediments.other = replace(support_access_impediments.other, 
                                             support %in% c("none","dnk"), NA)
    
  ) 


 all_questions_output[all_questions_output == ""] <- NA

 
 ##### Append GPS coordinates  ##### 
 
 gps_aggregation <- aggregate_gps(data)
 
 all_questions_output <- left_join(all_questions_output,gps_aggregation, by = "idp_code")

 ##### Joining data with the cccm master list and exporting the results ##### 
 
 master_list <- rio::import("input/CCCM list/IDP Site Master List  - 06 Oct 2021 .xlsx",sheet = 2) %>% 
   select(1,4) %>% 
   setNames(c("idp_code","settlement_name"))
 

final_output <- left_join(all_questions_output,master_list) %>% select(1:5,(ncol(.)-2):ncol(.),6:(ncol(.)-3))
 

write.csv(final_output,"output/Aggregation/DSA_aggregated_data.csv",
          na = "",row.names = F)



