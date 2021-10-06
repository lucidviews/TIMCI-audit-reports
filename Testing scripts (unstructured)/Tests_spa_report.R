.rs.restartR()
library(lubridate)
library(tidyquant)
library(dplyr)
library(readr)
library(ggplot2)
library(openxlsx)
library(knitr)
library(tibble)
library(stringr)
library(stringi)
library(readxl)
library(shiny)
library(plotly)
setwd("U:/Internship Swiss TPH/First week/TIMCI/Auto Rmd")
source("./Audit Report Functions.R", local = knitr::knit_global())

svc = 'https://timicodktest.smartforest.de/v1/projects/2/forms/02-TIMCI-SPA-CGEI.svc'
df = export_load_from_odk(svc)
df$question = sapply(df$node, create_question)
df$start_orig <- df$start
df$start <- format_date_ms(df$start)
df$end <- format_date_ms(df$end)

dfgrepl("-timeflow", 'https://timicodktest.smartforest.de/v1/projects/2/forms/07-TIMCI-timeflow.svc', fixed = T)

df5 = df %>%filter(event=='question')  %>% count(`instance ID`)
mean(df5$n)
summary(df)
length(unique(filter(df, event=='question')$question))
length(unique(filter(df, event=='question')$`instance ID`))


# splitting the df$node strings so that only the question name remains 
create_question <- function(str){
  key=tail(unlist(strsplit(str,"/")),1)
  return(key)
}

decode_question_test <- function(df, df_col, svc){
  
  setup_ruODK(svc)
  final_df <- data.frame(form_schema_ext())
  
  final_df = final_df %>% 
    filter(!grepl("generated_", name), !grepl('reserved_', name), type!='structure') 
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
df = decode_question_test(df, df$question, svc)

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

df1 = add_question_order(df, svc)
df1$question_order[15]=1
df1

add_order_violation <- function(df){
  df$order_violation = 0
  
  df = filter(df, !is.na(question_order))
  
  for (i in 2:nrow(df)){
    if (df$question_order[i]<df$question_order[i-1] & df$instance.ID[i]==df$instance.ID[i-1]) df$order_violation[i] = 1
  }
  return(df)
}

df1$question_order[6] = 2

df3 = add_order_violation(df1)
df1$question_order[7]=1

plot_sequence <- function(df, instance_id){
  pos <- c(0.3, -0.3, 0.5, -0.5, 0.7, -0.7)
  dir <- c(1, -1)
  
  df = df %>%
    filter(!is.na(question_order), instance.ID==instance_id)
  
  df = df %>%
    add_column('position'=rep(pos, length.out=nrow(df))) %>%
    add_column('direction'=rep(dir, length.out=nrow(df))) %>%
    mutate(idu=as.numeric(row.names(df))) %>%
    mutate(position_question = ifelse(position>0, position+0.07, position-0.03)) %>%
    mutate(position_question_order = ifelse(position_question>0, position+0.18, position-0.18))
  

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
  gg = gg + geom_text(aes(x=idu-0.2,y=-0.1,label=idu),size=2.5,vjust=0.5, color='black')
  gg = gg + geom_text(aes(y=position_question,label=question, col = ifelse(df$order_violation==1, 'red', 'darkgreen')),size=2.5)
  gg = gg + geom_text(aes(y=position_question_order, label=question_order, color='orange'), size=2.5)
  gg = gg + scale_color_identity(guide="legend", 
                                 breaks=c('red', 'darkgreen', 'orange'),
                                 labels=c('Original Order Violated', 'Original Order Ok', 'Original Order Number'),
                                 name='Original Order Explanation')

  print(gg)
}
plot_sequence(df3, 'uuid:2985578d-e410-4a14-bd31-f6813536d5c8')
df6 = plot_sequence(df1, 'uuid:96d473fd-b77c-4b7d-a121-70c08bb903d5')

df_violated = filter(df3, order_violation==1)

if (!is.null(df_violated)){
  ids_violated = unique(df_violated$instance.ID)
  
  for (id in ids_violated) plot_sequence(df3, id)
}

