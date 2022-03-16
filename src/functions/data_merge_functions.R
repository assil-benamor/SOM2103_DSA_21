library(tidyverse)
library(rlang)

# get current group
get_group <- function(df) {
  group <- group_vars(df)
  quo(unique(!!sym(group)))
}

# weights for data merge
dm_weights <- function(df, x, group_var = NULL, group = NULL) {
  
  weights <- weights(df)
  
  if (!is.null(group)) {
    weights <- weights[which(!is.na(df[[x]]) & df[[sym(group_var)]] == group)]
  }  else {
    weights <- weights[which(!is.na(df[[x]]))]
  }
  
  return(weights)
}





# weighted sum
weighted_sum <- function(x, df, x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  group_var <- group_vars(df)
  weights <- dm_weights(df, x_name, group_var, group)
  
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  
  x <- x[!is.na(x)]
  weights <- rep(1,length(x))
  
  if (length(x) == 0) {
    NA
  } else {
    sum(weights * x)
  }
}

q1_fn <- function(x, df, digits = 1, x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  group_var <- group_vars(df)
  weights <- dm_weights(df, x_name, group_var, group)
  
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  
  
  x <- x[!is.na(x)]
  
  return(as.numeric(summary(x)[2]))
  
  
  if (length(x) == 0) {
    NA
  } else {
    return(as.numeric(round(quantile(x)[[2]], digits)))
  }
}

q3_fn <- function(x, df, digits = 1, x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  group_var <- group_vars(df)
  weights <- dm_weights(df, x_name, group_var, group)
  
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  
  x <- x[!is.na(x)]
  
  
  if (length(x) == 0) {
    NA
  } else {
    return(as.numeric(round(quantile(x)[[4]], digits)))
    
  }
}

# weighted mean
weighted_mean <- function(x, df, digits = 1, x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  group_var <- group_vars(df)
  weights <- dm_weights(df, x_name, group_var, group)
  
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  
  x <- x[!is.na(x)]
  weights <- rep(1,length(x))
  
  if (length(x) == 0) {
    NA
  } else {
    round(sum(weights * x) / sum(weights), digits)
  }
}

# weighted median
weighted_median <- function(x, df, x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  
  group_var <- group_vars(df)
  weights <- dm_weights(df, x_name, group_var, group)
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  
  x <- x[!is.na(x)]
  weights <- rep(1,length(x))
  
  if (length(x) == 0) {
    NA
  } else {
    weights <- weights[order(x)]
    weights <- weights / sum(weights)
    sum_wts <- cumsum(weights)
    index <- min(which(sum_wts >= .5))
    x[index]
  }
}


# get percent response
percent_response <- function(x, df, ..., x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  group_var <- group_vars(df)

  weights <- dm_weights(df, x_name, group_var, group)
  args <- list(...)
  args <- unlist(args)
  args <- paste0("\\b", args, "\\b")
  args <- paste0("(", paste0(args, collapse = "|"), ")")
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  
  x[x==""] <- NA
  weights <- weights[!is.na(x)]
  
  x <- x[!is.na(x)]
  weights <- rep(1,length(x))
  
  if (length(x) == 0) {
    NA
  } else {
    pct <- sum(str_detect(x, args) * weights) / sum(weights)
    round(100 * pct, 1)
  }
}

num_percent_response <- function(x, df, ..., group = NULL) {
  x_name <- deparse(substitute(x))
  group_var <- group_vars(df)
  weights <- dm_weights(df, x_name, group_var, group)
  args <- list(...)
  args <- unlist(args)
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    NA
  } else {
    pct <- sum((x %in% args) * weights) / sum(weights)
    round(100 * pct, 1)
  }
}


# get percent of response

select_percents <- function(x, n, df, survey_sheet, choice_sheet, return_what, language = "english", exclude = NULL, x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  
  group_var <- group_vars(df)
  weights <- dm_weights(df, x_name, group_var, group)
  # basic setup
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    if (return_what == "label") {
      NA_character_
    } else {
      NA_integer_
    }
  } else {
    # Getting choices and labels
    l_name <- filter(survey_sheet, name == x_name)$type
    l_name <- str_remove(l_name, "(select_one |select_multiple )")
    choices <- filter(choice_sheet, list_name == l_name)$name
    
    if (!is.null(language)) {
      cols <- names(choice_sheet)
      col <- str_detect(cols, paste0("label[\\W]{2}(?i)", language))
      col <- cols[col]
    } else {
      col <- "label"
    }
    
    labels <- filter(choice_sheet, list_name == l_name)[[col]]
    # finding instances of choice options
    choice_rgx <- str_c("\\b", choices, "\\b")
    counts <- map_dbl(choice_rgx, ~sum(str_count(x, .x) * weights))
    
    if (!is.null(exclude)) {
      choices <- choices[order(counts, decreasing = T)]
      if(n == 1){
        while (choices[n] %in% exclude | is.na(choices[n])) {
          n <- n + 1
        }
      } else if (n == 2){
        while (choices[n] %in% exclude | choices[n-1] %in% exclude | is.na(choices[n])) {
          n <- n + 1
        }
      } else if (n == 3){
        while (choices[n] %in% exclude | choices[n-1] %in% exclude | choices[n-2] %in% exclude | is.na(choices[n])) {
          n <- n + 1
        }
      } else if (n == 4){
        while (choices[n] %in% exclude | choices[n-1] %in% exclude | choices[n-2] %in% exclude | choices[n-3] %in% exclude | is.na(choices[n])) {
          n <- n + 1
        }
      } else if (n == 5){
        while (choices[n] %in% exclude | choices[n-1] %in% exclude | choices[n-2] %in% exclude | choices[n-3] %in% exclude | choices[n-4] %in% exclude | is.na(choices[n])) {
          n <- n + 1
        }
      } else if (n == 6){
        while (choices[n] %in% exclude | choices[n-1] %in% exclude | choices[n-2] %in% exclude | choices[n-3] %in% exclude | choices[n-4] %in% exclude | choices[n-5] %in% exclude| is.na(choices[n])) {
          n <- n + 1
        }
      } 
      
      else {
        while (choices[n] %in% exclude | is.na(choices[n])) {
          n <- n + 1
        }
      }
    }
    
    if (return_what == "label") {
      labels <- labels[order(counts, decreasing = T)]
      labels[n]
    } else if (return_what == "percent") {
      counts <- counts[order(counts, decreasing = T)]
      round(100 * (counts[n] / sum(weights)), 1)
    }
  }
}

# spread multiple columns

big_spread <- function(df, key, value) {
  # quote key
  keyq <- enquo(key)
  # break value vector into quotes
  valueq <- enquo(value)
  s <- quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

# get names from ellipsis
ellipsis_names <- function(...) {
  args <- as.list(substitute(list(...)))[-1L]
  map_chr(args, deparse)
}

# concatenate columns into string based on value

str_concat <- function(val, ...) {
  col_names <- ellipsis_names(...)
  cols <- list(...)
  cols_match <- map(cols, str_detect, val)
  cols <- map2(cols_match, col_names, ~ ifelse(.x, .y, ""))
  string <- do.call(paste, cols) %>%
    str_remove_all("\\bNA\\b") %>%
    str_squish
  string[string == ""] <- NA
  string
}
