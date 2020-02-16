#### IMPORT THE PRETTY DATA PLAN ####

#### IMPORT AND CLEAN DATA ####

# Without DB
ImportDrakePlan <- drake_plan(
  
  # Meta, comm, trait and flux
  Data_CH = ImportClean_China(),
  Data_PE = ImportClean_Peru(),
  Data_SV = ImportClean_Svalbard(),
  Data_NO = ImportClean_Norway(),
  Data_CO = ImportClean_Colorado(),

  # Climate
  Climate = get(load(file = file_in("R/ClimateMetadata/MetaBioclimAllCountries.RData")))
)

# WIth DB
# ImportWDDrakePlan <- drake_plan(
#   
#   Data_CH = ImportClean_China(),
#   Data_PE = ImportClean_Peru(),
#   Data_SV = ImportClean_Svalbard(),
#   Data_NO = ImportClean_Norway(),
#   Data_CO = ImportClean_Colorado(),
#   Database0 = ImportClean_Database()
#   
# )