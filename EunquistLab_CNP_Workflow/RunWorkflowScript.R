####################################
### Run Enquist lab CNP workflow ###
####################################

# source all the functions
source("EunquistLab_CNP_Workflow/EnquistLab_CNP_Workflow.R")

# Run script
make_report(import_path_name = "isotope_data", file_name = "CNP_Results")
