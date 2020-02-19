####################################################################################
# Functions for load.R
####################################################################################


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


# Function to allocate grants to electorates
allocate_electorate <- function(points_ls, electorates_sf) {
  
  # Dataframe with SA1 names and placeholder electorate
  assign_df <- points_ls$dataframe %>% 
    mutate(electorate = 0)
  
  # Loop through point-in-polygon checking
  for (i in 1:length(points_ls$coords)) {
    
    # Print for progress tracking
    if (i %% 100 == 0) {print(i)}
    
    # Extract coord
    coord <- points_ls$coords[i]
    coord_state <- points_ls$dataframe$State[i]
    
    # Loop through electorates (in state if possible - for efficiency)
    # if (coord_state %in% unique(electorates_sf$state)) {
    #   
    #   # Get polygons in state only
    #   state_electorates_sf <- subset(electorates_sf, state == coord_state)
    #   
    #   # Loop through electorates to assign
    #   for (j in 1:length(state_electorates_sf@polygons)) {
    #     
    #     # Electorate name and polygon
    #     electorate_name <- state_electorates_sf$elect_div[j]
    #     electorate_poly <- subset(state_electorates_sf, elect_div == electorate_name)
    #     
    #     # Does it contain coord
    #     electorate_contains = gContains(electorate_poly, coord)
    #     
    #     if (electorate_contains == TRUE) {
    #       assign_df$electorate[i] = electorate_name
    #       break
    #     } 
    #     
    #   }
    #   
    # } else { # Not assigned to state, must check all electorates
      
      # Loop through electorates to assign
      for (j in 1:length(electorates_sf@polygons)) {
        
        # Electorate name and polygon
        electorate_name <- electorates_sf$elect_div[j]
        electorate_poly <- subset(electorates_sf, elect_div == electorate_name)
        
        # Does it contain coord
        electorate_contains = gContains(electorate_poly, coord)
        
        if (electorate_contains == TRUE) {
          assign_df$electorate[i] = electorate_name
          break
        }
      }
    # }
  }
  
  # assign_df <- assign_df %>% 
  #   filter(electorate != 0)
  
  return(assign_df)
}
