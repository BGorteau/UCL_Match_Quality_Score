# Import the libraries
library(worldfootballR)
library(dplyr)
library(tibble)
library(readr)
library(rvest)
library(httr)
library(progress)

#######################################
##### Get the list of matches url #####
#######################################

# Competition url
url <- "https://fbref.com/en/comps/8/2020-2021/schedule/2020-2021-Champions-League-Scores-and-Fixtures"
# Scrape the page
page <- read_html(url)
# Get matches table body
table_data <- page %>% html_node("tbody")
# Get the list of table rows
rows <- table_data %>% html_nodes("tr")
# Set up the vector for the matches url
matches_url <- c()
# Iterate on rows
for(r in rows){
  r_content <- r %>% html_nodes("td")
  match_url <- r_content[7] %>% html_nodes("a")
  if(length(match_url)==1){
    match_url <- match_url %>% html_attr("href")
    matches_url <- c(matches_url, paste0("https://fbref.com",match_url))
  }
}

# Set up a progress bar
pb <- progress_bar$new(
  format = "  [:bar] :percent ETA: :eta",
  total = length(matches_url), clear = FALSE, width=60
)

#############################
##### Retrieve the data #####
#############################

league <- "UCL"
year <- 2021

# Initialize the full dataframe
full_data <- tibble()
# Add each team data to the 'full_data'
for(match in matches_url){
  # Get the data type of the team
  match_data <- fb_advanced_match_stats(match_url=match, 
                                        stat_type="summary", 
                                        team_or_player="team")
  Sys.sleep(3)
  # Add the data
  full_data <- bind_rows(full_data, match_data)
  # Update the progress bar
  pb$tick()
}

#########################
##### Save the data #####
#########################

# Set up the repository
rep <- paste0("data/", league, "/", year,"/")

# Create the repo if it doesn't exist
if (!dir.exists(rep)){
  dir.create(rep)
}

# Save the data
write.csv(full_data, paste0(rep, "summary_stat.csv"), row.names = FALSE)
