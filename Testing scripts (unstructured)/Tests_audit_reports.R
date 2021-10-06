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

svc_sengal = 'https://research.odk.path.org/v1/projects/4/forms/02-TIMCI-SPA-cgei.svc'
svc_tanzania = 'https://timicodktest.smartforest.de/v1/projects/2/forms/02-TIMCI-SPA-CGEI.svc'

df = export_load_from_odk(svc)
df$question = sapply(df$node, create_question)
df$start <- format_date_ms(df$start)
df$end <- format_date_ms(df$end)
df <- decode_question(df, df$question, svc)
df <- decode_categories(df, svc)

schema_senegal_df <- data.frame(form_schema_ext())

setup_ruODK(svc_tanzania)
schema_tanzania_df <- data.frame(form_schema_ext())


