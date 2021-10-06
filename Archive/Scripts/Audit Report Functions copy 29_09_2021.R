library(dplyr)
library(readxl)
library(stringi)
library(readr)

test <- function(x, y){
  return(x+y)
}


# unzipping odk file and reading as csv
extract_data_from_odk_zip <- function(odk_zip, csv_name) {
  t <- tempdir()
  utils::unzip(odk_zip, exdir = t)
  fs::dir_ls(t)
  raw_odk_data <- readr::with_edition(1, readr::read_csv(file.path(t, csv_name)))
}

# formatting date from ms
format_date_ms <- function(df_col){
  return(as.POSIXct(df_col/1000, origin="1970-01-01"))
}


# splitting the df$node strings so that only the question name remains 
create_question <- function(str){
  key=tail(unlist(strsplit(str,"/")),1)
  return(key)
}


# decoding the question name 
decode_question <- function(df, df_col, codebook_excel){
  sheets <- readxl::excel_sheets(codebook_excel)
  sheets <- as.list(sheets)
  
  # looping over each sheet, dropping unnecessary cols and saving the data 
  final_df <- data.frame()
  for (i in 1:length(sheets)){
    if (nrow(final_df) == 0 ) {
      final_df <- readxl::read_excel(codebook_excel, sheet = sheets[[i]]) 
      if (sheets[[i]] != "Calculated") final_df <- final_df %>% select(name, label) %>% na.omit() else next
    } else{
      data_next <- read_excel(codebook_excel, sheet = sheets[[i]]) 
      if (sheets[[i]] != "Calculated") data_next <- data_next %>% select(name, label) %>% na.omit() else next
      final_df <-  rbind(final_df,  data_next)
    }
  }
  
  question_names <- final_df$name
  question_labels <- final_df$label
  
  for (i in 1:length(question_labels)){
    if (grepl("^\\w.{0,5}\\)", question_labels[i])) question_labels[i] <- unlist(stri_split_fixed(question_labels[i], ") ", n=2))[2]
    if (!grepl('\\d', question_names[i])) question_labels[i] <- question_names[i]
  }
  
  df$question_decoded <- NA
  
  for (row in 1:nrow(df)){
    df$question_decoded[row] = question_labels[match(df_col[row], question_names)]
  }
  return(df)
}

decode_question2 <- function(df, df_col, svc){
  
  setup_ruODK(svc)
  final_df <- data.frame(form_schema_ext())
  
  final_df = final_df %>% 
    filter(!grepl("generated_", name)) 
  question_names <- final_df$name
  question_labels <- final_df$label_english_.en.
  print(question_names)
  print(question_labels)
  
  for (i in 1:length(question_labels)){
    if (grepl("^\\w.{0,5}\\)", question_labels[i])) question_labels[i] <- unlist(stri_split_fixed(question_labels[i], ") ", n=2))[2]
    if (!grepl('\\d', question_names[i])) question_labels[i] <- question_names[i]
  }
  
  df$question_decoded <- NA
  
  for (row in 1:nrow(df)){
    df$question_decoded[row] = question_labels[match(df_col[row], question_names)]
  }
  return(df)
}


# decoding the answers that can be given to a categorical question
decode_categories <- function(df, codebook_excel){
  codebook_df <- readxl::read_excel(codebook_excel, sheet = "Categorical")
  
  for (i in 1:nrow(codebook_df)){
    if (is.na(codebook_df$name[i])) codebook_df$name[i] <- codebook_df$name[i-1]
  }
  categorical_questions <- unique(codebook_df$name)
  
  df_final <- df
  df_final$new_value_decoded <- NA
  df_final$old_value_decoded <- NA
  
  for (i in 1:nrow(df_final)){
    if (df_final$`new-value`[i] %in% c(0:100) & df_final$question[i] %in% categorical_questions){
      df_final$new_value_decoded[i] <- codebook_df$category[codebook_df$name==df_final$question[i] & codebook_df$value==df_final$`new-value`[i]]
    } else if (grepl("^\\d,", df_final$`new-value`[i]) & df_final$question[i] %in% categorical_questions){
      vec = c()
      for (j in unlist(strsplit(df_final$`new-value`[i], ", ", fixed=T))){
        ans = codebook_df$category[codebook_df$name==df_final$question[i] & codebook_df$value==j]
        vec = c(vec, ans)
      }
      df_final$new_value_decoded[i] <- toString(vec)
    } else df_final$new_value_decoded[i] <- df_final$`new-value`[i]
    
    if (df_final$old.value[i] %in% c(0:100) & df_final$question[i] %in% categorical_questions){
      df_final$old_value_decoded[i] <- codebook_df$category[codebook_df$name==df_final$question[i] & codebook_df$value==df_final$old.value[i]]
    } else if (grepl("^\\d,", df_final$old.value[i]) & df_final$question[i] %in% categorical_questions){
      vec = c()
      for (j in unlist(strsplit(df_final$old.value[i], ", ", fixed=T))){
        ans = codebook_df$category[codebook_df$name==df_final$question[i] & codebook_df$value==j]
        vec = c(vec, ans)
      }
      df_final$old_value_decoded[i] <- toString(vec)
    } else df_final$old_value_decoded[i] <- df_final$old.value[i]
  }
  
  return(df_final)
}

# decoding the answers that can be given to a categorical question
decode_categories2 <- function(df, svc){
  
  setup_ruODK(svc)
  ruODK_df <- data.frame(form_schema_ext())
  
  categorical_questions <- ruODK_df$name[!is.na(ruODK_df$choices_english_.en.)&ruODK_df$choices_english_.en.!='NULL']
  
  df_final <- df
  df_final$new_value_decoded <- NA
  df_final$old_value_decoded <- NA
  
  for (i in 1:nrow(df_final)){
    if (df_final$`new-value`[i] %in% c(0:100) & df_final$question[i] %in% categorical_questions){
      choices = ruODK_df$choices_english_.en.[ruODK_df$name==df_final$question[i]]
      val_index = match(df_final$`new-value`[i], choices[[1]]$values)
      df_final$new_value_decoded[i] <- choices[[1]]$labels[val_index]
    } else if (grepl("^\\d,", df_final$`new-value`[i]) & df_final$question[i] %in% categorical_questions){
      vec = c()
      for (j in unlist(strsplit(df_final$`new-value`[i], ", ", fixed=T))){
        choices = ruODK_df$choices_english_.en.[ruODK_df$name==df_final$question[i]]
        val_index = match(j, choices[[1]]$values)
        ans = choices[[1]]$labels[val_index]
        vec = c(vec, ans)
      }
      df_final$new_value_decoded[i] <- toString(vec)
    } else df_final$new_value_decoded[i] <- df_final$`new-value`[i]
    
    if (df_final$old.value[i] %in% c(0:100) & df_final$question[i] %in% categorical_questions){
      choices = ruODK_df$choices_english_.en.[ruODK_df$name==df_final$question[i]]
      val_index = match(df_final$old.value[i], choices[[1]]$values)
      df_final$old_value_decoded[i] <- choices[[1]]$labels[val_index]
    } else if (grepl("^\\d,", df_final$old.value[i]) & df_final$question[i] %in% categorical_questions){
      vec = c()
      for (j in unlist(strsplit(df_final$old.value[i], ", ", fixed=T))){
        choices = ruODK_df$choices_english_.en.[ruODK_df$name==df_final$question[i]]
        val_index = match(j, choices[[1]]$values)
        ans = choices[[1]]$labels[val_index]
        vec = c(vec, ans)
      }
      df_final$old_value_decoded[i] <- toString(vec)
    } else df_final$old_value_decoded[i] <- df_final$old.value[i]
  }
  
  return(df_final)
}


group_summarise_sort <- function(df, group_col, sum_stat, sum_col, sort_name, group_col2=NULL, group_cols=FALSE){
  if (!group_cols){
    df %>% 
      group_by({{group_col}}) %>% 
      summarise("{{sort_name}}" := {{sum_stat}}({{sum_col}})) %>% 
      arrange(desc({{sort_name}}))
  } else{
    df %>% 
      group_by({{group_col}}, {{group_col2}}) %>% 
      summarise("{{sort_name}}" := {{sum_stat}}({{sum_col}})) %>% 
      arrange(desc({{sort_name}}))
  }
}


