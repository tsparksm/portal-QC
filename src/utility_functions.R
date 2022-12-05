# Set parameters to display
parms <- c("Temperature", 
           "Dissolved Oxygen", 
           "Dissolved Oxygen, Field", 
           "pH", 
           "pH, Field",
           "Density", 
           "Salinity", 
           "Salinity, Field", 
           "Orthophosphate Phosphorus", 
           "Total Phosphorus", 
           "Ammonia Nitrogen", 
           "Nitrite + Nitrate Nitrogen", 
           "Total Nitrogen", 
           "Chlorophyll a", 
           "Chlorphyll, Field", 
           "Pheophytin a", 
           "Light Intensity (PAR)", 
           "Light Transmissivity", 
           "Total Suspended Solids", 
           "Surface Light Intensity (PAR)", 
           "Silica", 
           "Total Organic Carbon", 
           "Dissolved Organic Carbon", 
           "Fecal Coliform", 
           "Enterococcus")

# Define discrete data path
Z_drive <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/"
data_fpath <- paste(Z_drive, 
                    "MarinePortal", 
                    "WaterQuality", 
                    "Shiny", 
                    "discrete_data.rda", sep = "/")

# Load site data downloaded from Monitoring Portal
load_site <- function() {
  fpath <- here("data", "marine_sites.txt")
  read_tsv(fpath, 
           col_types = cols(`Site ID` = col_character(), 
                            `Site Name` = col_character(), 
                            Locator = col_character(), 
                            Latitude = col_double(), 
                            Longitude = col_double(), 
                            Shallow = col_logical(), 
                            `Site Type` = col_character(), 
                            Area = col_character())) %>% 
    rename(SiteID = `Site ID`, 
           SiteName = `Site Name`, 
           SiteType = `Site Type`) %>% 
    mutate(Shallow = if_else(is.na(Shallow), FALSE, TRUE))
}

# Update discrete data file w/ all available marine bottle data, save as .rda
update_discrete <- function() {
  Z_drive <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/"
  data_fpath <- paste(Z_drive, 
                      "MarinePortal", 
                      "WaterQuality", 
                      "Shiny", sep = "/")
  fpath <- paste(data_fpath, "discrete_data.csv", sep = "/")
  site_data <- load_site()
  sites <- site_data$Locator
  data("discrete_parms")
  parms <- discrete_parms$ParmName
  download_discrete(sites, parms, fpath, include_bad = TRUE)
  initial_data <- import_discrete(fpath)
  save(initial_data, 
       file = paste(data_fpath, "discrete_data.rda", sep = "/"))
}

# Load discrete data from .rda
load_discrete <- function() {
  Z_drive <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/"
  data_fpath <- paste(Z_drive, 
                      "MarinePortal", 
                      "WaterQuality", 
                      "Shiny", sep = "/")
  fpath <- paste(data_fpath, "discrete_data.rda", sep = "/")
  load(fpath)
  return(initial_data)
}

# Process discrete data - update values, sort
process_discrete <- function(discrete_data) {
  new_data <- discrete_data %>% 
    filter(ParmDisplayName %in% parms) %>% 
    transmute(Locator = Locator, 
              CollectDate = CollectDate, 
              Value = ifelse(!is.na(OverrideValue), 
                          OverrideValue, 
                          Value), 
              MDL = Mdl, 
              RDL = Rdl, 
              Depth = Depth, 
              Year = year(CollectDate), 
              Month = month(CollectDate), 
              SampleID = as.character(SampleId), 
              LabSampleNum = LabSampleNum, 
              Parameter = factor(ParmDisplayName, 
                                 levels = parms), 
              Units = Units, 
              Qualifier = QfrCode, 
              QualityID = QualityId, 
              NonDetect = grepl("<MDL", Qualifier), 
              URL = paste0("http://dnrp-apps2/Monitoring-Portal/Sample/Edit/?lsn=", 
                           LabSampleNum)) %>%  
    mutate(Value = ifelse(NonDetect, MDL, Value)) %>% 
    filter(!is.na(Value))
}
