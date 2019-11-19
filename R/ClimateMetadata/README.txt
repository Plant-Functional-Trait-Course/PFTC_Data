Querying Chelsa data for world carbon flux sites - layers downloaded from here:
http://chelsa-climate.org/downloads/
~1km resolution dataset
All BIOCLIM variables

ChelsaExtract.R merges all bioclimatic Chelsa layers within a folder called "chelsa", then combines it
with the metadata from the gradient analysis

ChelsaMerge.R is a probably useless script that merges intermediate files because when I wrote this I
didn't have enough memory on my computer to hold all the layers for the whole planet at one time. 

CRUExtract.R extracts data downloaded from the below link, in ncdf4 format named "vap.nc"

Vapor pressure and potential evaporation climate data from CRU 
https://crudata.uea.ac.uk/cru/data/hrg/

Vapor pressure deficit is computed from vapor pressure following these resources:

https://cgiarcsi.community/data/global-aridity-and-pet-database/
http://www.cgiar-csi.org/wp-content/uploads/2012/11/Zomer-et-al-2007-A-Global-Analysis-of-the-Hydrologic-Dimensions-of-Climate-Change-Mitigation-through-Afforestation-and-Reforestation.pdf
http://www.cgiar-csi.org/wp-content/uploads/2012/11/Zomer-et-al-2008-A-Spatial-Analysis-of-Global-Land-Suitability-for-Clean-Development-Mechanism-Affresotation-and-Reforestation.pdf

