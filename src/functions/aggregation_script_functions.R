




generate_from_binaries <- function(data,select_multiple_questions) {
  
  do.call("cbind",map(select_multiple_questions,function(question_name,data) {
    df <- data %>% select(grep(paste0("^",question_name,"\\."), names(data))) 
    colnames(df) <- gsub(paste0(question_name,"\\."),"",colnames(df))
    map2_df(df, names(df), ~  replace(.x, .x==1, .y) %>% replace(. == 0, NA)) %>% 
      unite(!!sym(question_name),names(df),remove=TRUE,na.rm=TRUE,sep=" ") %>% 
      as.data.frame() %>% 
      mutate_all(list(~na_if(.,""))) 
  },data)) 
}

fn_select_one_mode <- function(x) {
 
  if(all(is.na(x))){return(NA)}

  uniqx <- unique(na.omit(x))
  
  
  if (length(which(tabulate(match(x, uniqx)) == max(tabulate(match(x, uniqx))))) > 1) {
    return("NC")
  }
  
  uniqx[which.max(tabulate(match(x, uniqx)))]
  
}

fn_select_one_yes_prevalence <- function(x) {
  if(all(is.na(x))){return(NA)}
  
  ifelse(any(x=="yes"), 
         "yes",
         fn_select_one_mode(x))

}

one_sd_mean <- function(x) {
  if(all(is.na(x))){return(NA)}
  x <- na.omit(x)
  if(length(unique(x))==1){
    return(x[1])
  }
  ceiling(mean(x[abs(x - mean(x)) < sd(x)]))
}

is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

fn_select_one_mode_subset <- function(x,subset_var=NULL,role=NULL) {
  if(all(is.na(x))){return(NA)}
  
  if (hasArg(subset_var) & hasArg(role)) {
    
    if(length(which(subset_var %in% role))!=0) {
      x <- x[which(subset_var %in% role)]
    }
    
  }
  
  
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
  
}

one_sd_mean_subset <- function(x,subset_var=NULL,role=NULL) {
  
  if(all(is.na(x))){return(NA)}
  x <- na.omit(x)
  
  if (hasArg(subset_var) & hasArg(role)) {
    
    if(length(which(subset_var %in% role))!=0) {
      x <- x[which(subset_var %in% role)]
    }
    
  }
  
  if(length(unique(x))==1){
    return(x[1])
  }
  ceiling(mean(x[abs(x - mean(x)) < sd(x)]))
}

select_multiple_apply_constraints <- function(df) {
  
  ###################### The code below was generated using
  # l1 <- questions %>% 
  #   filter(grepl("select_multiple",type),!is.na(constraint),grepl("not",constraint)) %>% 
  #   select(name,constraint) %>% pull(name)
  # 
  # l2 <- questions %>% 
  #   filter(grepl("select_multiple",type),!is.na(constraint),grepl("not",constraint)) %>% 
  #   select(name,constraint) %>% pull(constraint)
  # 
  # append.elm <- function(vec,elm){
  #   c(vec,elm)
  # }
  # 
  # map2(l1,l2,function(x,y){
  #   code = c()
  #   code = append.elm(code,sprintf("#### %s #####",x))
  #   code = append.elm(code,sprintf("#### %s #####",y))
  #   code = append.elm(code,sprintf(""))
  #   code = append.elm(code,sprintf('rows_nums <- which( df %%>%% select(starts_with("%s") & !ends_with("%s")) %%>%% rowSums() >=1 )',x,sub('[^\"]+\"([^\"]+).*', '\\1', y)))
  #   code = append.elm(code,sprintf('df[rows_nums,"%s.%s"] <- 0',x,sub('[^\"]+\"([^\"]+).*', '\\1', y)))
  #   code = append.elm(code,sprintf(""))
  #   code = append.elm(code,sprintf(""))
  #   
  #   return(code %>% paste(.,sep = '\n'))
  # }) %>% unlist() %>%  paste(.,sep = '\n') %>% clipr::write_clip()
  ###################### 
  
  
  #### camp_structure #####
  #### not(selected(., "no") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("camp_structure") & !ends_with("no")) %>% rowSums() >=1 )
  df[rows_nums,"camp_structure.no"] <- 0
  
  
  #### cccm_management #####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(.,"no_management") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("cccm_management") & !ends_with("dnk")) %>% rowSums() >=1 )
  df[rows_nums,"cccm_management.dnk"] <- 0
  
  
  #### cccm_committees #####
  #### not(selected(., "no_committees") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("cccm_committees") & !ends_with("no_committees")) %>% rowSums() >=1 )
  df[rows_nums,"cccm_committees.no_committees"] <- 0
  
  
  #### decision_making_committees #####
  #### not(selected(., "none_of_the_above") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("decision_making_committees") & !ends_with("none_of_the_above")) %>% rowSums() >=1 )
  df[rows_nums,"decision_making_committees.none_of_the_above"] <- 0
  
  
  #### evictions_tenureagreement_holder #####
  #### not(selected(., "none") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("evictions_tenureagreement_holder") & !ends_with("none")) %>% rowSums() >=1 )
  df[rows_nums,"evictions_tenureagreement_holder.none"] <- 0
  
  
  #### factors_contributing_forced_evictions #####
  #### not(selected(., "na") and count-selected(.) > 1) or not(selected(., "pnta") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("factors_contributing_forced_evictions") & !ends_with("na")) %>% rowSums() >=1 )
  df[rows_nums,"factors_contributing_forced_evictions.na"] <- 0
  
  rows_nums <- which( df %>% select(starts_with("factors_contributing_forced_evictions") & !ends_with("pnta")) %>% rowSums() >=1 )
  df[rows_nums,"factors_contributing_forced_evictions.pnta"] <- 0
  
  
  #### nfi_items_available #####
  #### not(selected(., "none") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("nfi_items_available") & !ends_with("none")) %>% rowSums() >=1 )
  df[rows_nums,"nfi_items_available.none"] <- 0
  
  
  #### nfi_access_impediments #####
  #### not(selected(., "no_impediments") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("nfi_access_impediments") & !ends_with("no_impediments")) %>% rowSums() >=1 )
  df[rows_nums,"nfi_access_impediments.no_impediments"] <- 0
  
  
  #### water_treatment_methods #####
  #### not(selected(., "no_treatment") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("water_treatment_methods") & !ends_with("no_treatment")) %>% rowSums() >=1 )
  df[rows_nums,"water_treatment_methods.no_treatment"] <- 0
  
  
  #### water_access_barriers #####
  #### not(selected(., "no_problem") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("water_access_barriers") & !ends_with("no_problem")) %>% rowSums() >=1 )
  df[rows_nums,"water_access_barriers.no_problem"] <- 0
  
  
  #### sanitation_solidwastedisposal #####
  #### not(selected(., "not_managed") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("sanitation_solidwastedisposal") & !ends_with("not_managed")) %>% rowSums() >=1 )
  df[rows_nums,"sanitation_solidwastedisposal.not_managed"] <- 0
  
  
  #### sanitation_access_impediments #####
  #### not(selected(., "no_impediments") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("sanitation_access_impediments") & !ends_with("no_impediments")) %>% rowSums() >=1 )
  df[rows_nums,"sanitation_access_impediments.no_impediments"] <- 0
  
  
  #### hygiene_access_impediments #####
  #### not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("hygiene_access_impediments") & !ends_with("dnk")) %>% rowSums() >=1 )
  df[rows_nums,"hygiene_access_impediments.dnk"] <- 0
  
  
  #### health_facilities #####
  #### not(selected(., "no_health_facility") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("health_facilities") & !ends_with("no_health_facility")) %>% rowSums() >=1 )
  df[rows_nums,"health_facilities.no_health_facility"] <- 0
  
  
  #### health_services #####
  #### not(selected(., "none") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("health_services") & !ends_with("none")) %>% rowSums() >=1 )
  df[rows_nums,"health_services.none"] <- 0
  
  
  #### male_health_problems #####
  #### not(selected(., "no_health_issues") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("male_health_problems") & !ends_with("no_health_issues")) %>% rowSums() >=1 )
  df[rows_nums,"male_health_problems.no_health_issues"] <- 0
  
  
  #### female_health_problems #####
  #### not(selected(., "no_health_issues") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("female_health_problems") & !ends_with("no_health_issues")) %>% rowSums() >=1 )
  df[rows_nums,"female_health_problems.no_health_issues"] <- 0
  
  
  #### health_barriers #####
  #### not(selected(., "no_problem") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("health_barriers") & !ends_with("no_problem")) %>% rowSums() >=1 )
  df[rows_nums,"health_barriers.no_problem"] <- 0
  
  
  #### nutrition_distributions #####
  #### not(selected(., "none") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("nutrition_distributions") & !ends_with("none")) %>% rowSums() >=1 )
  df[rows_nums,"nutrition_distributions.none"] <- 0
  
  
  #### nutrition_services #####
  #### not(selected(., "no_problem") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("nutrition_services") & !ends_with("no_problem")) %>% rowSums() >=1 )
  df[rows_nums,"nutrition_services.no_problem"] <- 0
  
  
  #### education_facilities #####
  #### not(selected(., "no_available") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("education_facilities") & !ends_with("no_available")) %>% rowSums() >=1 )
  df[rows_nums,"education_facilities.no_available"] <- 0
  
  
  #### education_barriers_boys #####
  #### (not(selected(., “no_problem”) and count-selected(.) >1) and not(selected(., “dnk”) and count-selected(.) > 1)) and count-selected(.) <=3 #####
  
  rows_nums <- which( df %>% select(starts_with("education_barriers_boys") & !ends_with("(not(selected(., “no_problem”) and count-selected(.) >1) and not(selected(., “dnk”) and count-selected(.) > 1)) and count-selected(.) <=3")) %>% rowSums() >=1 )
  df[rows_nums,"education_barriers_boys.(not(selected(., “no_problem”) and count-selected(.) >1) and not(selected(., “dnk”) and count-selected(.) > 1)) and count-selected(.) <=3"] <- 0
  
  
  #### education_barriers_girls #####
  #### (not(selected(., “no_problem”) and count-selected(.) >1) and not(selected(., “dnk”) and count-selected(.) > 1)) and count-selected(.) <=3 #####
  
  rows_nums <- which( df %>% select(starts_with("education_barriers_girls") & !ends_with("(not(selected(., “no_problem”) and count-selected(.) >1) and not(selected(., “dnk”) and count-selected(.) > 1)) and count-selected(.) <=3")) %>% rowSums() >=1 )
  df[rows_nums,"education_barriers_girls.(not(selected(., “no_problem”) and count-selected(.) >1) and not(selected(., “dnk”) and count-selected(.) > 1)) and count-selected(.) <=3"] <- 0
  
  
  #### foodsecurity_access_barriers #####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("foodsecurity_access_barriers") & !ends_with("dnk")) %>% rowSums() >=1 )
  df[rows_nums,"foodsecurity_access_barriers.dnk"] <- 0
  
  
  #### foodsecurity_coping_food #####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1) and not(selected(., "no_action") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("foodsecurity_coping_food") & !ends_with("dnk")) %>% rowSums() >=1 )
  df[rows_nums,"foodsecurity_coping_food.dnk"] <- 0
  
  
  #### other_reason_coping #####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1) and not(selected(., "no_reason") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("other_reason_coping") & !ends_with("dnk")) %>% rowSums() >=1 )
  df[rows_nums,"other_reason_coping.dnk"] <- 0
  
  
  #### housing_property_incidences #####
  #### not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("housing_property_incidences") & !ends_with("dnk")) %>% rowSums() >=1 )
  df[rows_nums,"housing_property_incidences.dnk"] <- 0
  
  
  #### protection_incidents #####
  #### not(selected(., "no_incidents") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("protection_incidents") & !ends_with("no_incidents")) %>% rowSums() >=1 )
  df[rows_nums,"protection_incidents.no_incidents"] <- 0
  
  
  #### protection_incidents_place #####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("protection_incidents_place") & !ends_with("dnk")) %>% rowSums() >=1 )
  df[rows_nums,"protection_incidents_place.dnk"] <- 0
  
  
  #### insecure_areas #####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1) and not(selected(., "no_problems") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("insecure_areas") & !ends_with("dnk")) %>% rowSums() >=1 )
  df[rows_nums,"insecure_areas.dnk"] <- 0
  
  
  #### support #####
  #### not(selected(., "none") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("support") & !ends_with("none")) %>% rowSums() >=1 )
  df[rows_nums,"support.none"] <- 0
  
  
  #### support_access_impediments #####
  #### not(selected(., "no_impediments") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("support_access_impediments") & !ends_with("no_impediments")) %>% rowSums() >=1 )
  df[rows_nums,"support_access_impediments.no_impediments"] <- 0
  
  
  #### action_to_prevent #####
  #### not(selected(., "no_action") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("action_to_prevent") & !ends_with("no_action")) %>% rowSums() >=1 )
  df[rows_nums,"action_to_prevent.no_action"] <- 0
  
  
  #### aap_informationsources #####
  #### not(selected(., "no_info") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("aap_informationsources") & !ends_with("no_info")) %>% rowSums() >=1 )
  df[rows_nums,"aap_informationsources.no_info"] <- 0
  
  
  #### aap_informationsources_pwd #####
  #### not(selected(., "no_info") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("aap_informationsources_pwd") & !ends_with("no_info")) %>% rowSums() >=1 )
  df[rows_nums,"aap_informationsources_pwd.no_info"] <- 0
  
  
  #### aap_access_barriers #####
  #### not(selected(., "no_problem") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("aap_access_barriers") & !ends_with("no_problem")) %>% rowSums() >=1 )
  df[rows_nums,"aap_access_barriers.no_problem"] <- 0
  
  
  #### aap_humanitarianassistanceproblems #####
  #### not(selected(., "no_problems") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("aap_humanitarianassistanceproblems") & !ends_with("no_problems")) %>% rowSums() >=1 )
  df[rows_nums,"aap_humanitarianassistanceproblems.no_problems"] <- 0
  
  
  #### pwd_obstacles_complaints #####
  #### not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("pwd_obstacles_complaints") & !ends_with("dnk")) %>% rowSums() >=1 )
  df[rows_nums,"pwd_obstacles_complaints.dnk"] <- 0
  
  
  #### reason_nt_using_complaint_mechanisms #####
  #### not(selected(., "dnk") and count-selected(.) > 1) #####
  
  rows_nums <- which( df %>% select(starts_with("reason_nt_using_complaint_mechanisms") & !ends_with("dnk")) %>% rowSums() >=1 )
  df[rows_nums,"reason_nt_using_complaint_mechanisms.dnk"] <- 0
  
  
  #### aap_languages #####
  #### (not(selected(., "pnta") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)) and count-selected(.)<=3 #####
  
  rows_nums <- which( df %>% select(starts_with("aap_languages") & !ends_with("pnta")) %>% rowSums() >=1 )
  df[rows_nums,"aap_languages.pnta"] <- 0
  
  
  
  return(df)
}

# fn_select_one_mode_nc_correction <- function(x,subset_var=NULL,role=NULL) {
#   
#   if(all(is.na(x))){return(NA)}
#   
#   if (fn_select_one_mode(x) != 'NC') {
#     return(fn_select_one_mode(x))
#   }
#   
#   if (hasArg(subset_var) & hasArg(role)) {
#     
#     if(length(which(subset_var %in% role))!=0) {
#       x <- x[which(subset_var %in% role)]
#     }
#     
#   }
#   
#   
#   uniqx <- unique(na.omit(x))
#   uniqx[which.max(tabulate(match(x, uniqx)))]
#   
# }

dodo <- function(x,subset_var=NULL,role=NULL) {
  
  if(all(is.na(x))){return(NA)}
  
  
  if (fn_select_one_mode(x) != 'NC') {
    return(fn_select_one_mode(x))
  }
  
  if (hasArg(subset_var) & hasArg(role)) {
    
    if(length(which(subset_var %in% role))!=0) {
      x <- x[which(subset_var %in% role)]
    }
    
    
  }
  
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
  
}


fn_select_one_mode_nc_correction <- function(x,subset_var=NULL,role=NULL) {
  
  if(all(is.na(x))){return(NA)}
  
  
  if (fn_select_one_mode(x) != 'NC') {
      return(fn_select_one_mode(x))
  }
  
  if (hasArg(subset_var) & hasArg(role)) {
    
    if(length(which(subset_var %in% role))!=0) {
      x <- x[which(subset_var %in% role)]
    }
    

  }
  
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
  
}


fn_select_one_no_prevalence <- function(x) {
  
  if(all(is.na(x))){return(NA)}
  
  ifelse(any(x=="no"), 
         "no",
         fn_select_one_mode(x))
  
}

aggregate_gps <- function(df) {
  
  ## Sites with wrong or missing coordinates, gps coordintes to be pulled from the CCCM camps master list
  
  wrong_coordinates <- c("CCCM-SO2401-0513", "CCCM-SO2401-0200", "CCCM-SO2501-0005", "CCCM-SO2401-0028", "CCCM-SO1101-0011", "CCCM-SO1501-0002", "CCCM-SO2401-0299", "CCCM-SO1402-0003", "CCCM-SO1301-0023", "CCCM-SO2401-0080", "CCCM-SO1402-0005", "CCCM-SO1501-0011", "CCCM-SO2401-0330", "CCCM-SO2401-0114")
  missing_coordinates <-  c("CCCM-SO220117-0117", "CCCM-SO220117-0124", "CCCM-SO220117-0315", "CCCM-SO220117-0323", "CCCM-SO220117-0490", "CCCM-SO220117-0768", "CCCM-SO2801-0018", "CCCM-SO2801-0044", "CCCM-SO2801-0066", "CCCM-SO2801-0057", "CCCM-SO2801-0058", "CCCM-SO2801-0042", "CCCM-SO2801-0038", "CCCM-SO1501-0009", "CCCM-SO2401-0017", "CCCM-SO2401-0018", "CCCM-SO2401-0139", "CCCM-SO2401-0176", "CCCM-SO2401-0180", "CCCM-SO2401-0236", "CCCM-SO2401-0247", "CCCM-SO2401-0275", "CCCM-SO2401-0286", "CCCM-SO2401-0479", "CCCM-SO2401-0484", "CCCM-SO2401-0504", "CCCM-SO2401-0542")
  
  sites_to_correct <- c(wrong_coordinates,missing_coordinates)
  
  gps_coordinates <- readRDS("input/data/gps_coordinates.RDS")
  
  df <- left_join(df,gps_coordinates)
  
  df <- df %>% group_by(idp_code) %>% summarise(
    gps_latitude = mean(latitude,na.rm = T),
    gps_longitude = mean(longitude,na.rm = T)) %>%
    select(idp_code,gps_latitude,gps_longitude) %>% as.data.frame()
  
  cccm_master_list <- readRDS("input/data/cccm_master_list.RDS")
  
  
  df <- rbind(df %>% filter(!idp_code %in% sites_to_correct),
              cccm_master_list %>% filter(idp_code %in% sites_to_correct) 
  )
  
  return(df)
  
  
}

