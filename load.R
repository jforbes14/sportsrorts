# Load libraries
library(tidyverse)
library(rvest)
library(stringr)
library(eechidna)
library(ggmap)
library(sp)
library(rgeos)

# Load functions from functions.R
source("functions.R")

####################################################################################
# Scrape the locations where sports grants were allocated by government
# Source: https://www.sportaus.gov.au/grants_and_funding/community_sport_infrastructure_grant_program/successful_grant_recipient_list
####################################################################################

# Load table from aus sports as text
webpage_text <- read_html("https://www.sportaus.gov.au/grants_and_funding/community_sport_infrastructure_grant_program/successful_grant_recipient_list") %>% 
  html_nodes('td') %>% 
  html_text()

# Extract list of organisation, amount, state, round
orgs <- lapply(webpage_text[seq(1, length(webpage_text), 4)], clean_name) %>% unlist()
amounts <- lapply(webpage_text[seq(2, length(webpage_text), 4)], clean_dollar) %>% unlist()
states <- lapply(webpage_text[seq(3, length(webpage_text), 4)], str_trim) %>% unlist()
rounds <- lapply(webpage_text[seq(4, length(webpage_text), 4)], str_trim) %>% unlist()

# Wrangle to dataframe
grants_df <- data.frame(
  Organisation = orgs,
  Amount = amounts,
  State = states,
  Round = rounds
) %>% mutate(Search_Address = paste(Organisation, State))

save(grants_df, file = "grants_df.rda")

####################################################################################
# Geocode locations using GoogleMaps API
# You need to have an API from a Google cloud account (can get 12 month trial for free)
# https://cloud.google.com/maps-platform/
# Ensure you have the latest ggmap version from github
####################################################################################

# Enter your API key
register_google(key = my_api_key)

# Geocode
grants_geocoded_df <- grants_df %>% 
  mutate_geocode(Search_Address) %>% 
  rename(Latitude = lat, Longitude = lon)

# Manually fill in the NAs
grants_na_geocode <- grants_geocoded_df %>% 
  filter(is.na(Latitude))
grants_na_geocode$Latitude <- c(-33.8413565,-33.6061916, -37.595907, -37.7260833, -34.9727729,
  -34.9450696, -34.9196556, -34.9433813, -37.8273961, -41.1849933, -37.7844258, -38.0829467)
grants_na_geocode$Longitude <- c(138.5768095, 135.741567, 140.3395327, 140.5915723,  138.5990716,
  138.5104173, 138.4969033, 138.6468549, 140.7484311, 146.3349724, 145.131015, 144.3666039)

# Join back for final geocoded grants
grants_geocoded_final <- grants_geocoded_df %>% 
  filter(!is.na(Latitude)) %>% 
  bind_rows(grants_na_geocode)

####################################################################################
# Allocate each grant to an electorate using 2016 boundaries

# Substitute `grants_temp` with `grants_geocoded_final`
####################################################################################

# Load shapefile of 2016 electoral boundaries
sF_2016 <- eechidna::sF_download("2016")

# Create list of spatial points for grants, make format same as shapefile
grants_spatial_points <- grants_geocoded_final %>% ### Replace here ###
  select(Longitude, Latitude) %>% 
  SpatialPoints()
grants_spatial_points@proj4string <- sF_2016@proj4string

grants_spatial_ls <- list(dataframe = grants_geocoded_final %>% 
                            select(Organisation, Amount, State, Round), 
                          coords = grants_spatial_points)

# Allocate each grant to an electorate
grants_in_electorates <- allocate_electorate(grants_spatial_ls, sF_2016)
