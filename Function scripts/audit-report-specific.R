library(dplyr)
library(readr)
library(ggplot2)
library(openxlsx)
library(knitr)
library(tibble)
library(stringr)
library(stringi)
library(readxl)
library(lubridate)
library(shiny)
library(plotly)
library(ruODK)

# execute ODK Central settings
setup_ruODK <- function(svc){
  ruODK::ru_setup(
    svc = svc, 
    un = Sys.getenv('ODKC_UN'), 
    pw = Sys.getenv('ODKC_PW'),
    tz = Sys.getenv('ODKC_TZ'),
    verbose = TRUE
  )
}

# export data from ODK Central via RestAPI, store the retrieved zip file in a temporary directory and load the audit file as df 
export_load_from_odk <- function(svc){
  
  setup_ruODK(svc)
  
  t1 <- tempdir()
  
  ruODK::submission_export(t1)
  
  zip_path = fs::dir_ls(t1)[1]
  
  utils::unzip(zip_path, exdir = t1)
  
  csv_path = fs::dir_ls(t1)[1]
  
  df = data.frame(readr::read_csv(csv_path))
  
  return(df)
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


# adds a column that indicates at which position this event(question) is supposed to happen according to the form definition
add_question_order <- function(df, svc){
  
  setup_ruODK(svc)
  final_df <- data.frame(form_schema_ext())
  
  final_df = final_df %>%
    filter(grepl('^\\w\\d_.{0,12}', name) | name=='step_type' | name=='summary' | name=='visit_start' | name=='visit_end' | name=='child_identification' | name=='front_page')
  
  df$question_order = NA
  
  for (q in final_df$name){
    df$question_order[df$question==q] <- match(q, final_df$name)
  }
  
  return(df)
}


# flags events where the original form order was violated
add_order_violation <- function(df){
  df$order_violation = 0
  
  
  df = df %>%
    filter(!is.na(question_order)) %>%
    arrange(instance.ID, start_orig)
  
  for (i in 2:nrow(df)){
    if (df$question_order[i]<df$question_order[i-1] & df$instance.ID[i]==df$instance.ID[i-1]) df$order_violation[i] = 1
  }
  return(df)
}


# extracts the label for each event
decode_question <- function(df, df_col, svc){
  
  setup_ruODK(svc)
  final_df <- data.frame(form_schema_ext())
  
  final_df = final_df %>% 
    filter(!grepl("generated_", name)) 
  question_names <- final_df$name
  question_labels <- final_df$label_english_.en.
  
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
decode_categories <- function(df, svc){
  
  setup_ruODK(svc)
  ruODK_df <- data.frame(form_schema_ext())
  
  categorical_questions <- ruODK_df$name[!is.na(ruODK_df$choices_english_.en.)&ruODK_df$choices_english_.en.!='NULL']
  
  df_final <- df
  df_final$new_value_decoded <- NA
  df_final$old_value_decoded <- NA
  
  for (i in 1:nrow(df_final)){
    if (df_final$new.value[i] %in% c(0:100) & df_final$question[i] %in% categorical_questions){
      choices = ruODK_df$choices_english_.en.[ruODK_df$name==df_final$question[i]]
      val_index = match(df_final$new.value[i], choices[[1]]$values)
      df_final$new_value_decoded[i] <- choices[[1]]$labels[val_index]
    } else if (grepl("^\\d,", df_final$new.value[i]) & df_final$question[i] %in% categorical_questions){
      vec = c()
      for (j in unlist(strsplit(df_final$new.value[i], ", ", fixed=T))){
        choices = ruODK_df$choices_english_.en.[ruODK_df$name==df_final$question[i]]
        val_index = match(j, choices[[1]]$values)
        ans = choices[[1]]$labels[val_index]
        vec = c(vec, ans)
      }
      df_final$new_value_decoded[i] <- toString(vec)
    } else df_final$new_value_decoded[i] <- df_final$new.value[i]
    
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


# timeline plot that shows in which order a form was filled out
plot_sequence <- function(df, instance_id){
  pos <- c(0.3, -0.3, 0.5, -0.5)
  dir <- c(1, -1)
  
  df = df %>%
    filter(!is.na(question_order), instance.ID==instance_id) %>%
    arrange(start_orig, question_order)
  
  df = df %>%
    add_column('position'=rep(pos, length.out=nrow(df))) %>%
    add_column('direction'=rep(dir, length.out=nrow(df))) %>%
    mutate(idu=as.numeric(row.names(df))) %>%
    mutate(position_question = ifelse(position>0, position+0.1, position-0.05)) %>%
    mutate(position_question_order = ifelse(position_question>0, position+0.25, position-0.25))
  
  
  gg = ggplot(aes(x=idu, y=0, label=question), data=df)
  gg = gg + theme_classic()
  gg = gg + geom_hline(yintercept=0, color = "black", size=0.3)
  gg = gg + geom_segment(aes(y=position,yend=0,xend=idu), color='black', size=0.2)
  gg = gg + geom_point(aes(y=0), size=3)
  gg = gg + ylim(-3,3)
  gg = gg + theme(axis.line.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.x =element_blank(),
                  axis.ticks.x =element_blank(),
                  axis.line.x =element_blank())
  gg = gg + ggtitle(paste0(' Filling Order for Instance ', instance_id))
  gg = gg + geom_text(aes(x=idu-0.3,y=-0.15,label=idu),size=2.5,vjust=0.5, color='black')
  gg = gg + geom_text(aes(y=position_question,label=question, col = ifelse(df$order_violation==1, 'red', 'darkgreen')),size=2.5)
  gg = gg + geom_text(aes(y=position_question_order, label=question_order, color='orange'), size=2.5)
  gg = gg + scale_color_identity(guide="legend", 
                                 breaks=c('red', 'darkgreen', 'orange'),
                                 labels=c('Original Order Violated', 'Original Order Ok', 'Original Order Number'),
                                 name='Original Order Explanation')
  
  print(gg)
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


