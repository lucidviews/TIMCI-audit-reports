library(rmarkdown)

# Enter path to directory with zip files
zip_dir <- "U:/Internship Swiss TPH/First week/Data/Senegal"
zip_files <- list.files(path=zip_dir, full.names = TRUE)

# set working directory
setwd("U:/Internship Swiss TPH/First week/TIMCI/Auto Rmd")

# enter codebook paths
codebook_tflow <- "U:/Internship Swiss TPH/First week/Data/Tanzania/07-TIMCI-tflow (2)/07-TIMCI-timeflow_codebook.xlsx"
codebook_fa <- "U:/Internship Swiss TPH/First week/Data/Tanzania/03a-TIMCI-SPA-fa/03-TIMCI-SPA-FA-ref-form_codebook.xlsx"
codebook_cgei <- "U:/Internship Swiss TPH/First week/Data/Tanzania/02-TIMCI-SPA-cgei/02-TIMCI-SPA-CGEI-ref-form_codebook.xlsx"

# Shall the code be displayed?
display_code_chunks_global = F




insert_params <- function(zip, display_code, audit_type, codebook_path){
  path_splitted = unlist(strsplit(zip, ".", fixed=T))
  path_splitted_twice = unlist(strsplit(path_splitted[1], "/", fixed=T))
  return(
    list(file_path_zip = zip, 
         file_name_csv = paste0(tail(path_splitted_twice, 1),  " - audit.csv"), 
         display_code_chunks = display_code,
         set_title = paste0(tail(path_splitted_twice, 1), " Audit Analysis"),
         type = audit_type,
         codebook = codebook_path)
  )
}

render_rmd <- function(rmd_file_path = "./Audit Report Template.Rmd",
                       output_format = 'html_document', 
                       output_file, 
                       output_dir = ".",
                       params){
  rmarkdown::render(input=rmd_file_path, output_format = output_format, output_file = output_file, output_dir = output_dir, params = params, envir = new.env())
}

find_audit_category <- function(zip, display_code){
  if (grepl("-tflow", zip, fixed=T)){
    render_rmd(output_file = paste0("timeflow_audit_analysis_",format(Sys.time(), '%d-%b-%Y'), ".html"), 
               params = insert_params(zip = zip, display_code = display_code, audit_type = "tflow", codebook_path = codebook_tflow))
  } else if (grepl("-fa", zip, fixed=T)){
    render_rmd(output_file = paste0("facility_assessment_audit_analysis_",format(Sys.time(), '%d-%b-%Y'), ".html"), 
               params = insert_params(zip = zip, display_code = display_code, audit_type = "fa", codebook_path = codebook_fa))
  } else if (grepl("-cgei", zip, fixed=T)){
    render_rmd(output_file = paste0("caregiver_audit_analysis_",format(Sys.time(), '%d-%b-%Y'), ".html"), 
               params = insert_params(zip = zip, display_code = display_code, audit_type = "cgei", codebook_path = codebook_cgei))
  } else {
    stop("zip file does not match category")
  }
}

for (zip_path in zip_files){
  if (display_code_chunks_global){
    find_audit_category(zip=zip_path, display_code = T)
  } else {
    find_audit_category(zip=zip_path, display_code = F)
  } 
}

