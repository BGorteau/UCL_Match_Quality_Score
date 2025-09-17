# Import the libraries
library(worldfootballR)
library(dplyr)
library(tibble)
library(readr)
library(rvest)
library(httr)
library(progress)

################################
##### SET UP THE VARIABLES #####
################################

# Base stats (match info)
base_stats <- c("League", "Match_Date", "Matchweek", "Team", "Home_Away", 
                "Home_Team", "Away_Team", "Home_Score", "Away_Score", 
                "Home_Formation", "Away_Formation", "season")

# List of KPIs/Stats to compute the MQS

KPI_list_shots <- c("npxG_Expected", # Non penalty expected goals
                    "SoT" # Shots on Target
                    )
                    
KPI_list_dangerous_actions <- c("xAG_Expected", # Expected Assisted Goals
                                "SCA_SCA", # Shot-Creating Actions
                                "GCA_SCA" # Goal-Creating Actions
                                )      

KPI_list_possession <- c("PrgP_Passes", # Progressive Passes
                         "PrgC_Carries" # Progressive Carries
                         )

KPI_list_defense <- c(
                      # "Att_Take_Ons", # Takes-Ons attempted
                      "Succ_Take_Ons", # Successful Takes-Ons
                      "Tkl", # Tackles
                      "Int", # Interceptions
                      "Blocks" # Blocks
                      )

KPI_list <- c(KPI_list_shots, KPI_list_dangerous_actions, KPI_list_possession, 
              KPI_list_defense)

# KPI_list <- c(
#   # SHOTS
#   "npxG_Expected", # Non penalty expected goals
#   "SoT", # Shots on Target
#   # DANGEROUS ACTIONS
#   "xAG_Expected", # Expected Assisted Goals
#   "SCA_SCA", # Shot-Creating Actions
#   "GCA_SCA", # Goal-Creating Actions
#   # POSSESSION
#   "PrgP_Passes", # Progressive Passes
#   "PrgC_Carries", # Progressive Carries
#   # DEFENSE
#   "Att_Take_Ons", # Takes-Ons attempted
#   "Succ_Take_Ons", # Successful Takes-Ons
#   "Tkl", # Tackles
#   "Int", # Interceptions
#   "Blocks" # Blocks
# )

# KPI_list <- c("Gls", "Ast", "Sh", "SoT", "Touches", "Tkl", "Int", "Blocks", 
#               "xG_Expected", "npxG_Expected", "xAG_Expected", "SCA_SCA", 
#               "GCA_SCA", "Cmp_Passes", "Att_Passes", "Cmp_percent_Passes", 
#               "PrgP_Passes", "Carries_Carries", "PrgC_Carries", "Att_Take_Ons", 
#               "Succ_Take_Ons")

################################
##### SET UP THE FUNCTIONS #####
################################

# Put stats per 90/min

stats_per_90_min <- function(stats_df, KPI_list){
  # Create a 'minutes multiplication' column
  minutes_multiplication <- c()
  for(mins in stats_df[["Min"]]){
    if(mins <= 90){
      minutes_multiplication <- c(minutes_multiplication, 1)
    }else{
      minutes_multiplication <- c(minutes_multiplication, 90/120)
    }
  }
  # Add the column to the dataframe
  stats_df$minutes_multiplication <- minutes_multiplication
  
  # Multiply the stats by the 'minutes multiplication'
  for(stat in KPI_list){
    stats_df[stat] <- stats_df[stat]*stats_df$minutes_multiplication
  }
  
  # RETURN
  return(stats_df)
}

# Function to compute the score
give_a_rank <- function(stats_vector, stat){
  # Compute the percentile of values bellow
  pct <- ecdf(stats_vector)(stat) * 100
  # RETURN
  return(pct)
}

# Compute the MQS for each team
compute_team_MQS <- function(match_stats, matchs_to_compare, 
                             base_stats=base_stats, KPI_list=KPI_list){
  # Set up a new dataframe
  MQS_team_data <- match_stats[1, base_stats]
  # Add the team formation
  home_away <- MQS_team_data[[1, "Home_Away"]]
  if(home_away == "Home"){
    MQS_team_data["Formation"] <- MQS_team_data[[1, "Home_Formation"]]
  }else{
    MQS_team_data["Formation"] <- MQS_team_data[[1, "Away_Formation"]]
  }
  
  # Add the basic stats
  for(stat in KPI_list){
    MQS_team_data[stat] <- give_a_rank(matchs_to_compare[[stat]], 
                                       match_stats[[1, stat]])
  }
  
  # Mean scores
  # Team shots MQS
  MQS_team_data["team_Shots_MQS"] <- mean(as.vector(unlist(
    MQS_team_data[1, KPI_list_shots])))
  # Team dangerous actions MQS
  MQS_team_data["team_DA_MQS"] <- mean(as.vector(unlist(
    MQS_team_data[1, KPI_list_dangerous_actions])))
  # Team possession MQS
  MQS_team_data["team_Poss_MQS"] <- mean(as.vector(unlist(
    MQS_team_data[1, KPI_list_possession])))
  # Team defense MQS
  MQS_team_data["team_Def_MQS"] <- mean(as.vector(unlist(
    MQS_team_data[1, KPI_list_defense])))
  # Team MQS
  MQS_team_data["team_MQS"] <- mean(as.vector(unlist(
    MQS_team_data[1, KPI_list])))
  # RETURN
  return(MQS_team_data)
}

# Compute MQS for all teams
compute_all_teams_MQS <- function(all_matchs_stats, matchs_to_compare, 
                                  bs=base_stats, KPI_l=KPI_list){
  # Set up the final dataframe
  full_data <- tibble()
  # Add stats to the final dataframe
  for(i in seq(nrow(all_matchs_stats))){
    full_data <- bind_rows(full_data,
                           compute_team_MQS(all_matchs_stats[i, ], 
                                            matchs_to_compare, 
                                            base_stats=bs, 
                                            KPI_list=KPI_l))
  }
  # RETURN
  return(full_data)
}

# Compute the MQS for matches based on teams MQS
compute_matches_MQS <- function(team_matches_MQS){
  # Set up the final dataframe
  games_MQS <- tibble()
  # Compute the WQS for each match
  for(i in seq(nrow(team_matches_MQS))){
    # Matchweek
    matchweek <- team_matches_MQS[[i,"Matchweek"]]
    # Date
    match_date <- team_matches_MQS[[i,"Match_Date"]]
    # Team 1
    team_1 <- team_matches_MQS[[i,"Home_Team"]]
    # Team 2
    team_2 <- team_matches_MQS[[i,"Away_Team"]]
    # Home team score
    Home_Score <- team_matches_MQS[[i,"Home_Score"]]
    # Away team score
    Away_Score <- team_matches_MQS[[i,"Away_Score"]]
    # Score
    match_score <- paste0(team_matches_MQS[[i,"Home_Score"]], " - ", team_matches_MQS[[i,"Away_Score"]])
    # Comp
    comp <- team_matches_MQS[[i,"League"]]
    # Season
    season <- team_matches_MQS[[i,"season"]]
    # Filter the rows of all_teams_MQS
    match_data <- team_matches_MQS %>% 
      filter((Match_Date == match_date)&(Matchweek == matchweek)) %>%
      filter(
        ((Home_Team == team_1)&(Away_Team == team_2))|
          ((Home_Team == team_2)&(Away_Team == team_1))
      )
    # Add home team to final data
    if("Neutral" %in% match_data$Home_Away){
      # Get teams names
      home_team <- match_data[[1,"Team"]]
      away_team <- match_data[[2,"Team"]]
      # Get teams MQS
      home_team_MQS <- match_data[[1,"team_MQS"]]
      away_team_MQS <- match_data[[2,"team_MQS"]]
    }else{
      # Get teams names
      home_team <- (match_data %>% filter(Home_Away=="Home"))[[1,"Team"]]
      away_team <- (match_data %>% filter(Home_Away=="Away"))[[1,"Team"]]
      # Get teams MQS
      home_team_MQS <- (match_data %>% filter(Home_Away=="Home"))[[1,"team_MQS"]]
      away_team_MQS <- (match_data %>% filter(Home_Away=="Away"))[[1,"team_MQS"]]
    }
    # Final match data
    final_data <- tibble(
      H_Team = c(home_team),
      A_Team = c(away_team),
      Home_Score = c(Home_Score),
      Away_Score = c(Away_Score),
      Score = c(match_score),
      Matchweek = c(matchweek),
      date = c(match_date),
      competition = c(comp),
      season = c(season),
      H_Team_MQS = c(home_team_MQS),
      A_Team_MQS = c(away_team_MQS),
      MQS = c(mean(match_data$team_MQS))
    )
    # Bind rows
    games_MQS <- bind_rows(games_MQS, final_data)
  }
  # Keep unique games
  games_MQS <- games_MQS %>% distinct()
  # RETURN
  return(games_MQS)
}

###########################
##### IMPORT THE DATA #####
###########################

# Set up the final dataframe
full_data <- tibble()
# List of competitions to get the data from
comps <- c("UCL")
# List of years
years <- c(2021, 2022, 2023, 2024, 2025)

# Iterate on the files
for(comp in comps){
  for(y in years){
    # Set up the file path
    file_path <- paste0("data/",comp,"/",y,"/summary_stat.csv")
    if(file.exists(file_path)){
      # Import season data
      season_data <- read_csv(file_path, show_col_types = FALSE)
      # Add the season years to the dataframe
      season_data$season <- rep(paste0(y-1,"-",y), nrow(season_data))
      # Add the season data to the full dataframe
      full_data <- bind_rows(full_data, season_data)
    }
  }
}

##############################
##### COMPUTE THE SCORES #####
##############################

full_data <- stats_per_90_min(full_data, KPI_list)

teams_MQS <- compute_all_teams_MQS(full_data, full_data)

matches_MQS <- compute_matches_MQS(teams_MQS)

print(matches_MQS %>% arrange(-MQS))

#########################
##### SAVE THE DATA #####
#########################

# Save 'team_MQS' data
write_csv(teams_MQS, "data/metrics_data/teams_MQS.csv")
# Save 'matches_MQS' data
write_csv(matches_MQS, "data/metrics_data/matches_MQS.csv")
