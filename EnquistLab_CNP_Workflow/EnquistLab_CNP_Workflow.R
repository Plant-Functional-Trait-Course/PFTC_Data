#################################
### Enquist lab CNP workflow ###
#################################

# LOAD LIBRARIES
#devtools::install_github("audhalbritter/PFTCFunctions")
#devtools::install_github("tidyverse/googlesheets4")
library("tidyverse")
library("lubridate")
library("googlesheets4")
library("readxl")
library("R.utils")
library("broom")
library("googledrive")
library("devtools")
library("PFTCFunctions")


pn <- . %>% print(n = Inf)


############################################################################
#### ENVELOPE CODES ####
# Get unique hashcodes (Peru: seed = 1; Svalbard: seed = 32)

# Create list with all valid IDs per country
creat_ID_list <- function(){
  all_codes <- get_PFTC_envelope_codes(seed = 1) %>% 
    mutate(Site = "Peru") %>% 
    bind_rows(get_PFTC_envelope_codes(seed = 32) %>% 
                mutate(Site = "Svalbard"))
  return(all_codes)
}


############################################################################
#### PHOSPHORUS DATA ####
# Download Phosphorus data from google sheet
import_phosphorus_data <- function(){
  cnp <- gs_title("CNP_Template")
  p <- gs_read(ss = cnp, ws = "Phosphorus") %>% as_tibble()
  return(p)
}


# pull of standard, calculate R2, choose standard for absorbance curve, make regression and plot
get_standard <- function(p){
  standard_concentration <- tibble(Standard = c(0, 2, 4, 8, 12, 16),
                                       Concentration = c(0, 0.061, 0.122, 0.242, 0.364, 0.484))
  
  Standard <- p %>% 
    select(Site, Batch, Individual_Nr, Sample_Absorbance) %>% 
    filter(Individual_Nr %in% c("Standard1", "Standard2"),
           # remove batch if Sample_Absorbance is NA; Sample has not been measured
           !is.na(Sample_Absorbance)) %>% 
    group_by(Site, Batch, Individual_Nr) %>% 
    # not a good solution!!!
    mutate(n = n()) %>% 
    filter(n == 6) %>% 
    nest(standard = c(Sample_Absorbance)) %>% 
    mutate(standard = map(standard, bind_cols, standard_concentration)) 
  
  return(Standard)
}

  
# Plot 2 Standard curves
plot_standards <- function(Standard){
  p1 <- Standard %>% 
    unnest() %>% 
    ggplot(aes(x = Sample_Absorbance, y = Concentration, colour = Individual_Nr)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Absorbance", y = expression(paste("Concentration ", mu, "g/ml")))
  
  return(p1)
}


# Choose standard and make model
standard_model <- function(Standard){
  ModelResult <- Standard %>% 
    mutate(correlation = map_dbl(standard, ~cor(.$Sample_Absorbance, .$Concentration, use = "pair"))) %>% 
    group_by(Site, Batch) %>% 
    slice(which.max(correlation)) %>% 
    mutate(fit = map(standard, ~lm(Concentration ~ Sample_Absorbance, .)))
  return(ModelResult)
}



# Calculate Mean, sd, coeficiant variability for each leaf and flag data
original_phosphor_data <- function(p, ModelResult){
  p2 <- p %>% 
    filter(!Individual_Nr %in% c("Standard1", "Standard2"),
           # remove samples without mass
           !is.na(Sample_Mass)) %>% 
    group_by(Site, Batch) %>% 
    nest(data = c(Individual_Nr:Name_measured)) %>% 
    # add estimate from model
    left_join(ModelResult %>% select(-Individual_Nr), by = "Batch")
  
    # remove if correlation is NA, but give warning
    # if(nrow(p2 %>% filter(is.na(correlation))) > 0){
    #   p2 <- p2 %>% 
    #     filter(!is.na(correlation))
    # }
  
  OriginalValues <- p2 %>% 
      mutate(data = map2(.x = data, .y = fit, ~ mutate(.x, Sample_yg_ml = predict(.y, newdata = select(.x, Sample_Absorbance))))) %>% 
    unnest(data) %>% 
    mutate(Pmass = Sample_yg_ml * Volume_of_Sample_ml,
           Pconc = Pmass / Sample_Mass * 100) %>% 
    # Calculate mean, sd, coefficient of variation
    group_by(Batch, Site, Individual_Nr) %>% 
    mutate(meanP = mean(Pconc, na.rm = TRUE), 
           sdP = sd(Pconc, na.rm = TRUE),
           CoeffVarP = sdP / meanP) %>% 
    # flag data
    mutate(Flag_orig = ifelse(CoeffVarP >= 0.2, "flag", ""))
  
  return(OriginalValues)
}


# wheat: check values, flag/remove, calculate convertion factor
calculate_correction_factor <- function(OriginalValues, RedWheatValue = 0.137){
  
  CorrectionFactor <- OriginalValues %>% 
    filter(Individual_Nr %in% c("Hard Red Spring Wheat Flour")) %>% 
    mutate(P_Correction = Pconc / RedWheatValue) %>% 
    # Calculate mean, sd, coefficient of variation
    group_by(Batch, Site, Individual_Nr) %>% 
    summarise(Correction_Factor = mean(P_Correction, na.rm = TRUE)) %>% 
    select(-Individual_Nr)
  return(CorrectionFactor)
  
}


# Use Correction Factor on data
corrected_phosphor_data <- function(OriginalValues, CorrectionFactor){
  CorrectedValues <- OriginalValues %>% 
    filter(!Individual_Nr %in% c("Hard Red Spring Wheat Flour")) %>% 
    left_join(CorrectionFactor, by = c("Batch", "Site")) %>% 
    mutate(Pconc_Corrected = Pconc * Correction_Factor) %>% 
    # Calculate mean, sd, coefficient of variation
    group_by(Batch, Site, Individual_Nr) %>% 
    mutate(meanP_Corrected = mean(Pconc_Corrected, na.rm = TRUE), 
           sdP_Corrected = sd(Pconc_Corrected, na.rm = TRUE),
           CoeffVarP_Corrected = sdP_Corrected / meanP_Corrected,
           N_replications = n()) %>% 
    # flag data
    mutate(Flag_corrected = ifelse(CoeffVarP_Corrected >= 0.2, "flag", ""))
  return(CorrectedValues)
}


### Check IDs
checkIDs_P <- function(CorrectedValues, all_codes){
  NotMatching <- CorrectedValues %>% 
    anti_join(all_codes, by = c("Individual_Nr" = "hashcode", "Site" = "Site")) %>% 
      select(Batch, Site, Individual_Nr)
  return(NotMatching)

}

############################################################################
#### ISOTOPE DATA ####
# download isotope data from google drive
download_isotope_data <- function(){
  path <- "EnquistLab_CNP_Workflow/IsotopeData"
  list_of_files <- drive_ls(path = path, pattern = "xlsx") 
  
  map2(.x = list_of_files$id,
         .y = list_of_files$name,
         ~ drive_download(as_id(.x), path = file.path("isotope_data", .y), overwrite = TRUE))
  invisible()
}


# import CN and isotope data and merge
import_cn_data <- function(import_path_name){
  # Import CN mass from CNP data set
  cnp <- gs_title("CNP_Template")
  cn_mass <- gs_read(ss = cnp, ws = "CN") %>% 
    mutate(Samples_Nr = as.character(Samples_Nr)) %>% 
    as_tibble()
  
  # Read isotope data
  list_files <- dir(path = import_path_name, pattern = "\\.xlsx$", full.names = TRUE)
  cn_isotopes <- map(list_files, read_excel, skip = 13) %>% 
    map_df(~{select(.,-c(...12:...17)) %>% 
        slice(1:grep("Analytical precision, 1-sigma", ...1)-1) %>% 
        filter(!is.na(...1)) %>% 
        rename(Samples_Nr = ...1, Individual_Nr = `Sample ID`, Site = ...3, Row = R, Column = C, C_percent = `C%`, N_percent = `N%`, CN_ratio = `C/N`, dN15_percent = `δ15N ‰(ATM)`, dC13_percent = `δ13C ‰(PDB)`, Remark_CN = ...11)
      })
  
  cn_data <- cn_mass %>% 
    full_join(cn_isotopes, by = c("Samples_Nr", "Individual_Nr", "Site", "Row", "Column")) %>% 
    rename(Row_cn = Row, Column_cn = Column)
  return(cn_data) 
}


cn_d <- cn_mass %>% 
  full_join(cn_isotopes, by = c("Samples_Nr", "Individual_Nr", "Site", "Row", "Column"))
cn_d %>% 
  filter(is.na(Sample_Mass) | is.na(C_percent))
# add date measured
#mutate(date_measured = date)

# Is this needed?
check_cn_data <- function(cn_data){
  # check not matching ids
  not_matching_ids <- cn_data %>% 
    filter(is.na(Sample_Mass) | is.na(C_percent))
  return(not_matching_ids)
}

# join all tables
merge_cnp_data <- function(cn_data, CorrectedValues){
  cnp_data <- cn_data %>% 
    full_join(CorrectedValues, by = c("Individual_Nr", "Site", "Batch"))
  return(cnp_data)
}


### Check IDs from cn dataset
checkIDs_cn_mass <- function(all_codes){
  cnp <- gs_title("CNP_Template")
  cn_mass <- gs_read(ss = cnp, ws = "CN") %>% 
    mutate(Samples_Nr = as.character(Samples_Nr)) %>% 
    as_tibble()
  
  NotMatching <- cn_mass %>% 
    anti_join(all_codes, by = c("Individual_Nr" = "hashcode", "Site" = "Site")) %>% 
    select(Batch, Site, Individual_Nr)
  return(NotMatching)
  
}

checkIDs_cn_isotope <- function(all_codes){
  # Read isotope data
  list_files <- dir(path = import_path_name, pattern = "\\.xlsx$", full.names = TRUE)
  cn_isotopes <- map(list_files, read_excel, skip = 13) %>% 
    map_df(~{select(.,-c(...12:...17)) %>% 
        slice(1:grep("Analytical precision, 1-sigma", ...1)-1) %>% 
        filter(!is.na(...1)) %>% 
        rename(Samples_Nr = ...1, Individual_Nr = `Sample ID`, Site = ...3, Row = R, Column = C, C_percent = `C%`, N_percent = `N%`, CN_ratio = `C/N`, dN15_percent = `δ15N ‰(ATM)`, dC13_percent = `δ13C ‰(PDB)`, Remark_CN = ...11)
    })
  
  NotMatching <- cn_isotopes %>% 
    anti_join(all_codes, by = c("Individual_Nr" = "hashcode", "Site" = "Site")) %>% 
    select(Site, Individual_Nr)
  return(NotMatching)
  
}


############################################################################
#### MAKE REPORT ####
make_report <- function(import_path_name, file_name = NULL){
  # if(!missing(batch_nr)){
  #   Standard <- Standard %>% 
  #     filter(Batch == batch_nr)
  # }
  
  # run all functions
  RedWheatValue <- 0.137
  all_codes <- creat_ID_list()
  
  p <- import_phosphorus_data()
  standard <- get_standard(p)
  plot_standards(standard)
  ModelResult <- standard_model(standard)
  
  OriginalValues <- original_phosphor_data(p, ModelResult)
  CorrectionFactor <- calculate_correction_factor(OriginalValues, RedWheatValue = RedWheatValue)
  CorrectedValues <- corrected_phosphor_data(OriginalValues, CorrectionFactor)
  WrongIDs_P <- checkIDs_P(CorrectedValues, all_codes)
  
  # import cn and isotope data, merge the two data sets
  message("Downloading isotope data")
  download_isotope_data()
  
  cn_data <- import_cn_data(import_path_name)
  
  # check wrong IDs
  WrongIDs_cn_mass <- checkIDs_cn_mass(all_codes)
  WrongIDs_cn_mass <- checkIDs_cn_mass(all_codes)
  
  # not sure what this does!!!?
  check_cn <- check_cn_data(cn_data)
  
  # merge cnp data and output
  cnp <- merge_cnp_data(cn_data, CorrectedValues)
  
  if(!is.null(file_name)){
    write_csv(x = cnp, path = paste0("EnquistLab_CNP_Workflow/cnp_results/", file_name, ".csv"))
  }
  
  standard %>% 
    distinct(Batch) %>% 
    arrange(Batch) %>% 
    slice(1:2) %>% 
    pull(Batch) %>% 
    map(~rmarkdown::render("EnquistLab_CNP_Workflow/EnquistLab_CNP_Workflow.Rmd", params = list(batch_nr = .), output_dir = "EnquistLab_CNP_Workflow/Results", output_file = paste0("Results_CNP_Workflow_", ., ".pdf")))
}

### To do!!!
# staple all pdfs together
# staplr::staple_pdf(input_directory = "EnquistLab_CNP_Workflow/Results", output_filepath = paste0("EnquistLab_CNP_Workflow/Results/", "Full_pdf.pdf"))