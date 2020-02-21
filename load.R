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
####################################################################################

# Load shapefile of 2016 electoral boundaries
sF_2016 <- eechidna::sF_download("2016")

# Create list of spatial points for grants, make format same as shapefile
grants_spatial_points <- grants_geocoded_final %>% 
  select(Longitude, Latitude) %>% 
  SpatialPoints()
grants_spatial_points@proj4string <- sF_2016@proj4string

grants_spatial_ls <- list(dataframe = grants_geocoded_final, 
                          coords = grants_spatial_points)

# Allocate each grant to an electorate
grants_in_electorates <- allocate_electorate(grants_spatial_ls, sF_2016)

####################################################################################
# Correct grants without an electorate (due to errors with geocoding)
####################################################################################

correct_these <- grants_in_electorates %>% 
  filter(electorate == 0)

# For reference, the venues corresponding are held in the text file "grants without electorate.txt"
# search_addresses <- c('Parkes Tennis Club Inc NSW', 'Paramount Tennis Club Inc. NSW', 'Portland Golf Club Ltd NSW', 
#   'St Peter and St Paul Coptic Orthodox Church NSW', 'Sandgate Hawks Sporting Club QLD', 'Gladstone District Dirtriders Club Inc QLD', 
#   'Tennis Gold Coast Inc QLD', 'North Eastern Sports Shooting Club Inc. SA', 'St Marys Park Sports Association Incorporated SA', 
#   'Latrobe Council TAS', 'Dover RSL Bowls Club Inc TAS', 'Wynyard Golf Club TAS', 'Phillip Island District Cricket Club VIC', 
#   'Murrumbeena Tennis Club VIC', 'North Box Hill Tennis Club Inc VIC', 'Somers Yacht Club Inc VIC', 'City of Cockburn WA', 
#   'Modernians Hockey Club Inc WA', 'Western Australian Rugby Union Inc WA', 'Westonia Golf Club Inc WA', 'Brunswick Tennis Club Inc WA', 
#   'Peel Thunder Football Club Inc. WA', 'Modernians Hockey Club Inc WA', 'Mount Walker Sports Club Inc. WA', 'St. Annes Primary School WA')

correct_these$Latitude <- c(-33.1323045, -32.2497074, -33.3613608, -34.4789956, -27.3357627, -23.8414146, -27.9723754, -34.8168072,
                            -35.0075105, -41.2340353, -43.3155021, -40.9896649, -38.450023, -37.8925233, -37.7991689, -38.3935331,
                            -32.1195254, -31.9310798, -31.9484753, -31.1769563, -33.2530811, -32.5330023, -31.9310798, -27.7604462,
                            -33.083067)
correct_these$Longitude <- c(148.1658072, 148.6058157, 149.980211, 150.8409754, 153.0415517, 151.2391907, 153.4152462, 138.7513504,
                             138.576817, 146.4145415, 147.0147585, 145.7230645, 145.2376704, 145.0683927, 145.1227317, 145.1549587,
                             115.8387652, 115.8477556, 115.7821763, 118.3625379, 115.8347886, 115.7378564, 115.8477556, 152.509644,
                             115.889246)

# Run electorate allocation on these only
# Create list of spatial points for grants, make format same as shapefile
corrected_points <- correct_these %>% 
  select(Longitude, Latitude) %>% 
  SpatialPoints()
corrected_points@proj4string <- sF_2016@proj4string

corrected_points_ls <- list(dataframe = correct_these, 
                          coords = corrected_points)

# Allocate each grant to an electorate
corrected_points_electorates <- allocate_electorate(corrected_points_ls, sF_2016)

# Manual electorate allocation for those still with no electorate
corrected_points_electorates$electorate[which(corrected_points_electorates$Search_Address == "Phillip Island District Cricket Club VIC")] <- "BASS"
corrected_points_electorates$electorate[which(corrected_points_electorates$Search_Address == "North Box Hill Tennis Club Inc VIC")] <- "CHISHOLM"

# Combine for final grant-electorate dataframe
grants_electorates_df <- grants_in_electorates %>% 
  filter(electorate != 0) %>% 
  bind_rows(
    corrected_points_electorates
  )

####################################################################################
# Create dataframe that holds electorates, grant information and some socio-demographics 
####################################################################################

# Aggregate grants for each electorate
electorate_df <- grants_electorates_df %>% 
  rename(Electorate = electorate) %>% 
  group_by(Electorate) %>% 
  summarise(Number_Grants = n(), Amount = sum(Amount))

# Join ABS, election results with aggregated grants
df <- abs2016 %>% 
  select(DivisionNm, Population, MedianAge, MedianPersonalIncome, 
         HighSchool, Unemployed, Owned) %>% 
  left_join(tpp16 %>% 
              select(DivisionNm, LNP_Percent, Swing), by = "DivisionNm") %>% 
  left_join(electorate_df, by = c("DivisionNm" = "Electorate")) %>% 
  mutate(Number_Grants = replace_na(Number_Grants, 0),
         Amount = replace_na(Amount, 0))

# Save
save(df, file = "df.rda")
