library(tidyverse)
library(rvest)
library(stringr)
library(eechidna)
library(ggmap)

####################################################################################
# Scrape the locations where sports grants were allocated by government
# Source: https://www.sportaus.gov.au/grants_and_funding/community_sport_infrastructure_grant_program/successful_grant_recipient_list
####################################################################################

# Load table from aus sports as text
webpage_text <- read_html("https://www.sportaus.gov.au/grants_and_funding/community_sport_infrastructure_grant_program/successful_grant_recipient_list") %>% 
  html_nodes('td') %>% 
  html_text()

# Clean dollar amounts function
clean_dollar <- function(str) {
  return(
    str %>% 
      str_trim() %>% 
      str_remove("[$]") %>% 
      str_remove(",") %>% 
      as.numeric()
  )
}

# Clean organisation name function
clean_name <- function(str) {
  return(
    str %>% 
      str_trim() %>% 
      str_remove("\r\n ")
  )
}

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


####################################################################################
# Allocate each grant to an electorate
####################################################################################

####################################################################################
# 
####################################################################################
