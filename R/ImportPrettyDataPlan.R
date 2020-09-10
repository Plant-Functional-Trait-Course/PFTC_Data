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
  Climate = read_excel(file_in("data/MetaClimate.xlsx"))

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