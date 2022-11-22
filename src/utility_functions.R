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

# Update discrete data file w/ all available marine bottle data
update_discrete <- function() {
  fpath <- here("data", "discrete_data.csv")
  site_data <- load_site()
  sites <- site_data$Locator
  data("discrete_parms")
  parms <- discrete_parms$ParmName
  download_discrete(sites, parms, fpath, include_bad = TRUE)
  initial_data <- import_discrete(fpath)
  save(initial_data, 
       file = here("data", "discrete_data.rda"))
}

# Load discrete data
load_discrete <- function() {
  fpath <- here("data", "discrete_data.rda")
  load(fpath)
  return(initial_data)
}
