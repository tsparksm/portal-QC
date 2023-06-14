# Set parameters to display
parms <- c("Temperature", 
           "Density", 
           "Salinity", 
           "Salinity, Field", 
           "Dissolved Oxygen", 
           "Dissolved Oxygen, Field", 
           "pH", 
           "pH, Field",
           "Orthophosphate Phosphorus", 
           "Total Phosphorus", 
           "Ammonia Nitrogen", 
           "Nitrite + Nitrate Nitrogen", 
           "Total Nitrogen", 
           "Silica", 
           "Total Suspended Solids", 
           "Light Transmissivity", 
           "Light Intensity (PAR)", 
           "Surface Light Intensity (PAR)", 
           "Chlorophyll a", 
           "Chlorophyll, Field", 
           "Pheophytin a", 
           "Total Organic Carbon", 
           "Dissolved Organic Carbon", 
           "Fecal Coliform", 
           "Enterococcus"
)

# Set parameter units for axis labels
parm_units <- tibble(Parameter = parms, 
                     Label = c("Temperature (\u00B0C)", 
                               "Density (kg/m<sup>3</sup>)", 
                               "Salinity (PSU)", 
                               "Salinity, Field (PSU)", 
                               "Dissolved Oxygen (mg/L)", 
                               "Dissolved Oxygen, Field (mg/L)", 
                               "pH", 
                               "pH, Field", 
                               "Orthophosphate Phosphorus (mg/L)", 
                               "Total Phosphorus (mg/L)", 
                               "Ammonia Nitrogen (mg/L)", 
                               "Nitrate + Nitrite Nitrogen (mg/L)", 
                               "Total Nitrogen (mg/L)", 
                               "Silica (mg/L)", 
                               "Total Suspended Solids (mg/L)", 
                               "Light Transmissivity (%)", 
                               "PAR (\u00B5mol/sm<sup>2</sup>)", 
                               "Surface PAR (\u00B5mol/sm<sup>2</sup>)", 
                               "Chlorophyll <i>a</i> (\u00B5g/L)", 
                               "Chlorophyll <i>a</i>, Field (\u00B5g/L)", 
                               "Pheophytin <i>a</i> (\u00B5g/L)", 
                               "Total Organic Carbon (mg/L)", 
                               "Dissolved Organic Carbon (mg/L)", 
                               "Fecal Coliform (CFU/100 mL)", 
                               "Enterococcus (CFU/100 mL)"))

# Set Central Basin sites (tab 2)
locators_cb <-  c("JSUR01", "KSBP01", "CK200P", "KSSK02", 
                  "LTBC43", "LTED04", "SEAQYSI", "LSEP01", 
                  "LSKQ06", "LSNT01", "PTWILLBUOY", "LSVV01", 
                  "MSJN02", "MSWH01", "NSAJ02", "NSEX01")

# Set Whidbey Basin sites (tab 3)
locators_wb <- c("SARATOGACH", "PSUSANBUOY", 
                 "PENNCOVEENT", "PENNCOVECW", "PENNCOVEWEST")

# Identify parameters to have log-scaling
parms_log <- c("Orthophosphate Phosphorus", 
               "Total Phosphorus", 
               "Ammonia Nitrogen", 
               "Nitrite + Nitrate Nitrogen", 
               "Total Nitrogen", 
               "Total Suspended Solids", 
               "Silica")

# Define discrete data path
Z_drive <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/"
data_fpath <- paste(Z_drive, 
                    "MarinePortal", 
                    "WaterQuality", 
                    "ShinyOffshore", 
                    "discrete_data.rda", sep = "/")
site_fpath <- paste(Z_drive, 
                    "MarinePortal", 
                    "WaterQuality", 
                    "ShinyOffshore", 
                    "marine_sites.txt", sep = "/")

# Update site data downloaded from Monitoring Portal
# There is no option to do this from the app; needs to be manual update
update_site <- function() {
  if (is.na(Sys.getenv("site_user", unset = NA))) {
    Sys.setenv(site_user = getPass::getPass(msg = "Enter username"))
    Sys.setenv(site_pw = getPass::getPass(msg = "Enter password"))
  }
  
  webpage <- "http://dnrp-apps2/Monitoring-Portal/Sites?SiteType=2&pageSize=1000"
  
  tt <- RCurl::getURL(webpage, 
                      userpwd = paste(Sys.getenv("site_user"), 
                                      Sys.getenv("site_pw"), 
                                      sep = ":"))
  sites <- XML::readHTMLTable(tt, 
                              stringsAsFactors = FALSE)[[1]]
  colnames(sites) <- c("Details", "SiteID", "SiteName", "Locator", "Latitude", 
                       "Longitude", "Shallow", "SiteType", "Area")
  sites <- sites %>% 
    select(!Details) %>% 
    mutate(Shallow = if_else(is.na(Shallow), FALSE, TRUE))
  write_tsv(sites, site_fpath)
}

# Load site data downloaded from Monitoring Portal
load_site <- function() {
  read_tsv(site_fpath, 
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
                           LabSampleNum), 
              WeekDate = paste("Week", 
                               isoweek(CollectDate), 
                               "-" , 
                               month.abb[month(CollectDate)], 
                               year(CollectDate))) %>%  
    mutate(Value = ifelse(NonDetect, MDL, Value)) %>% 
    filter(!is.na(Value))
}
