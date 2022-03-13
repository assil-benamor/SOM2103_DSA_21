
data_merge_gen <- function(data,questionnaire,questions,choices,group,df_name,df_output_name) {
  x <- c()
  ch = sprintf("%s <- data %%>%%",df_output_name)
  x <- c(x,ch)
  ch= sprintf("group_by(%s) %%>%%  summarize( ",group)
  x <- c(x,ch)
  liste_of_questions <- colnames(data)
  liste_of_questions <- liste_of_questions[!str_detect(liste_of_questions,"\\.")]
  
  to_exclude <- questions %>% filter(!grepl(pattern = "(^select_|^calculate$|^integer$)",type)) %>% 
    pull(name)%>% unique()
  
  to_exclude <- c(to_exclude,"call_location")
  
  liste_of_questions <- liste_of_questions[!liste_of_questions %in% to_exclude]
  
  for (question in liste_of_questions) {
    if (question %in% questions$name) {
      if (questionnaire$question_is_categorical(question)) {
        list_of_choices <- questionnaire$question_get_choices(question)
        for (choice in list_of_choices) {
          ch = sprintf("%s_%s = percent_response(%s, .,'%s', group = !!get_group(.)),",
                  question,
                  choice,
                  question,
                  choice)
          x <- c(x,ch)
        }
        
        if(length(list_of_choices)>3){
          for (i in 1:4) {
            ch = sprintf("%s_top_%d_name = select_percents(%s, %d, ., questions, choices, 'label', group = !!get_group(.)),",
                         question,i,question,i)
            x <- c(x,ch)
            ch = sprintf("%s_top_%d_pct = select_percents(%s, %d, ., questions, choices, 'percent', group = !!get_group(.)),",
                         question,i,question,i)
            x <- c(x,ch)

          }
        }
      }
      
      else if (questionnaire$question_is_numeric(question)) {
        ch = sprintf("%s = weighted_mean(x = %s,df = .,digits = 2,group = !!get_group(.)),",
                     question,
                     question
                     )
        x <- c(x,ch)
        
        # ch = sprintf("%s_q1 = q1_fn(x = %s,df = .,digits = 2,group = !!get_group(.)),",
        #              question,
        #              question
        # )
        # x <- c(x,ch)
        # 
        # ch = sprintf("%s_q3 = q3_fn(x = %s,df = .,digits = 2,group = !!get_group(.)),",
        #              question,
        #              question
        # )
        # x <- c(x,ch)
        # 
        
      }
      
    }
    else{
      if (str_detect(question,pattern = "(_nc_index|_need$)") &
          !str_detect(question,pattern = "\\.")) {
        for (i in 0:1) {
          ch = sprintf("%s_%s = percent_response(%s, .,'%s', group = !!get_group(.)),",
                       question,
                       i,
                       question,
                       i)
          x <- c(x,ch)
        }
      }
      
      else if (str_detect(question,pattern = "(_index|number_of_needs|_score)") &
               !str_detect(question,pattern = "\\.")) {
        scores <- data %>% pull(question) %>% unique() %>% sort()
        for (i in scores) {
          ch = sprintf("%s_%s = percent_response(%s, .,'%s', group = !!get_group(.)),",
                       question,
                       i,
                       question,
                       i)
          x <- c(x,ch)
        }
      }
    }
  }
  
  
  y <- x %>% paste(.,sep = '\n')
  y[length(y)] <- gsub(",$","",y[length(y)] )
  y[length(y)+1] <- ")"
  y
  
}





data_merge_gen_v2 <- function(data,questionnaire,questions,liste_of_questions,choices,group,df_name,df_output_name) {
  x <- c()
  ch = sprintf("%s <- data %%>%%",df_output_name)
  x <- c(x,ch)
  ch= sprintf("group_by(%s) %%>%%  summarize( ",group)
  x <- c(x,ch)
  
  
  
  # liste_of_questions <- colnames(data)
  # liste_of_questions <- liste_of_questions[!str_detect(liste_of_questions,"\\.")]
  # to_exclude <- questions %>% filter(!grepl(pattern = "(^select_|^calculate$|^integer$)",type)) %>% 
  #   pull(name)%>% unique()
  # 
  # to_exclude <- c(to_exclude,"call_location")
  # 
  # liste_of_questions <- liste_of_questions[!liste_of_questions %in% to_exclude]
  
  
  for (question in liste_of_questions) {
    if (question %in% questions$name) {
      if (questionnaire$question_is_categorical(question)) {
        list_of_choices <- questionnaire$question_get_choices(question)
        
        if (l$type[match(question,l$question)] == 'pct') {
          for (choice in list_of_choices) {
            ch = sprintf("%s_%s = percent_response(%s, .,'%s', group = !!get_group(.)),",
                         question,
                         choice,
                         question,
                         choice)
            x <- c(x,ch)
          }
        }
        
        if(l$type[match(question,l$question)] == 'top3') {
          if(length(list_of_choices)>3){
            for (i in 1:4) {
              ch = sprintf("%s_top_%d_name = select_percents(%s, %d, ., questions, choices, 'label', group = !!get_group(.)),",
                           question,i,question,i)
              x <- c(x,ch)
              ch = sprintf("%s_top_%d_pct = select_percents(%s, %d, ., questions, choices, 'percent', group = !!get_group(.)),",
                           question,i,question,i)
              x <- c(x,ch)
              
            }
          }
        }
        
  
      }
      
      else if (questionnaire$question_is_numeric(question)) {
        
        if(l$type[match(question,l$question)] == 'mean') {
          ch = sprintf("%s = weighted_mean(x = %s,df = .,digits = 2,group = !!get_group(.)),",
                       question,
                       question
          )
          x <- c(x,ch)
        }
        else  if(l$type[match(question,l$question)] == 'sum') {
          ch = sprintf("%s = weighted_sum(x = %s,df = .,group = !!get_group(.)),",
                       question,
                       question
          )
          x <- c(x,ch)
        }
        
        else  if(l$type[match(question,l$question)] == 'median') {
          ch = sprintf("%s = weighted_median(x = %s,df = .,group = !!get_group(.)),",
                       question,
                       question
          )
          x <- c(x,ch)
        }
       
        
      }
      
    }
    else{
      if (str_detect(question,pattern = "(_nc_index|_need$)") &
          !str_detect(question,pattern = "\\.")) {
        for (i in 0:1) {
          ch = sprintf("%s_%s = percent_response(%s, .,'%s', group = !!get_group(.)),",
                       question,
                       i,
                       question,
                       i)
          x <- c(x,ch)
        }
      }
      
      else if (str_detect(question,pattern = "(_index|number_of_needs|_score)") &
               !str_detect(question,pattern = "\\.")) {
        scores <- data %>% pull(question) %>% unique() %>% sort()
        for (i in scores) {
          ch = sprintf("%s_%s = percent_response(%s, .,'%s', group = !!get_group(.)),",
                       question,
                       i,
                       question,
                       i)
          x <- c(x,ch)
        }
      }
    }
  }
  
  
  y <- x %>% paste(.,sep = '\n')
  y[length(y)] <- gsub(",$","",y[length(y)] )
  y[length(y)+1] <- ")"
  y
  
}
