.rs.restartR()
library(rmarkdown)

# set working directory
setwd("C:/Users/silblu/Documents/GitHub/TIMCI-reports")

# Enter ODK SVC
svc <- 'https://timicodktest.smartforest.de/v1/projects/2/forms/02-TIMCI-SPA-CGEI.svc'

# Shall the code be displayed?
display_code_chunks_global = F

svc_multiple = F




insert_params <- function(svc, display_code, audit_type){
  svc_splitted = tail(unlist(strsplit(svc, ".", fixed=T)), 2)[1]
  svc_splitted_twice = tail(unlist(strsplit(svc_splitted, "/", fixed=T)), 1)
  return(
    list(
         display_code_chunks = display_code,
         set_title = paste0(tail(svc_splitted_twice, 1), " Audit Analysis"),
         type = audit_type,
         svc = svc)
  )
}

render_rmd <- function(rmd_file_path = "./Markdown scripts/Audit-Report-Template-v2.Rmd", 
                       output_format = 'html_document', 
                       output_file, 
                       output_dir = "./Archive/Rendered RMDs",
                       params){
  rmarkdown::render(input=rmd_file_path, output_format = output_format, output_file = output_file, output_dir = output_dir, params = params, envir = new.env())
}

# adjust regex to be more general
find_audit_category <- function(svc, display_code){
  if (grepl("-timeflow", svc, fixed=T)){
    render_rmd(output_file = paste0("timeflow_audit_analysis_",format(Sys.time(), '%d-%b-%Y'), ".html"), 
               params = insert_params(svc = svc, display_code = display_code, audit_type = "tflow"))
  } else if (grepl("-Fassmt|-fassmt|-Fa|-fa", svc, fixed=T)){
    render_rmd(output_file = paste0("facility_assessment_audit_analysis_",format(Sys.time(), '%d-%b-%Y'), ".html"), 
               params = insert_params(svc = svc, display_code = display_code, audit_type = "fa"))
  } else if (grepl("-CGEI", svc, fixed=T)){
    render_rmd(output_file = paste0("caregiver_audit_analysis_",format(Sys.time(), '%d-%b-%Y'), ".html"), 
               params = insert_params(svc = svc, display_code = display_code, audit_type = "cgei"))
  } else{
    stop("svc does not match category")
  }
}

if (svc_multiple){
  for (svc in svc){
    if (display_code_chunks_global){
      find_audit_category(svc=svc, display_code = T)
    } else find_audit_category(svc=svc, display_code = F)
    }
  } else {
    if (display_code_chunks_global){
      find_audit_category(svc=svc, display_code = T)
    } else find_audit_category(svc=svc, display_code = F)
}

