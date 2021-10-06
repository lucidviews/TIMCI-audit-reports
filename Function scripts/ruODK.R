library(readr)
library(knitr)
library(readxl)
library(ruODK)
library(fs)

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
export_load_audit_from_odk <- function(svc){
  
  setup_ruODK(svc)
  
  t1 <- tempdir()
  
  ruODK::submission_export(t1)
  
  zip_path = fs::dir_ls(t1)[1]
  
  utils::unzip(zip_path, exdir = t1)
  
  csv_path = fs::dir_ls(t1)[1]
  
  df = data.frame(readr::read_csv(csv_path))
  
  return(df)
}


