# Import the libraries
library(dplyr)
library(tibble)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(gt)
library(readr)
library(sysfonts)
library(showtext)
library(pagedown)

#####################
##### ADD FONTS #####
#####################

font_add_google("Libre Franklin", "libre_franklin")
showtext_auto()

#######################
##### IMPORT DATA #####
#######################

teams_MQS <- read_csv("data/metrics_data/teams_MQS.csv", 
                      show_col_types = FALSE)
matches_MQS <- read_csv("data/metrics_data/matches_MQS.csv", 
                        show_col_types = FALSE)

#############################################
##### APPLY TRANSFORMATIONS TO THE DATA #####
#############################################

# Change matchweek column

## Matches MQS
new_matchweek <- c()
# Get matchweek
for(i in matches_MQS[["Matchweek"]]){
  # First split
  first_split <- strsplit(i, "\\(")[[1]][2]
  # Second split
  second_split <- strsplit(first_split, "\\)")[[1]][1]
  # Add new matchweek to the vector
  new_matchweek <- c(new_matchweek, second_split)
}
matches_MQS$Matchweek <- new_matchweek

## Teams MQS
new_matchweek <- c()
# Get matchweek
for(i in teams_MQS[["Matchweek"]]){
  # First split
  first_split <- strsplit(i, "\\(")[[1]][2]
  # Second split
  second_split <- strsplit(first_split, "\\)")[[1]][1]
  # Add new matchweek to the vector
  new_matchweek <- c(new_matchweek, second_split)
}
teams_MQS$Matchweek <- new_matchweek

# Add if the game is a win or loss in teams_MQS + add the opponent
W_L <- c()
opponent <- c()
for(i in seq(nrow(teams_MQS))){
  # Home team
  if(teams_MQS[[i, "Home_Away"]]=="Home"){
    # Add the opponent
    opponent <- c(opponent, teams_MQS[[i, "Away_Team"]])
    # Win
    if(teams_MQS[[i, "Home_Score"]] > teams_MQS[[i, "Away_Score"]]){
      W_L <- c(W_L, "W")
    }
    # Loss
    if(teams_MQS[[i, "Home_Score"]] < teams_MQS[[i, "Away_Score"]]){
      W_L <- c(W_L, "L")
    }
    # Draw
    if(teams_MQS[[i, "Home_Score"]] == teams_MQS[[i, "Away_Score"]]){
      W_L <- c(W_L, "D")
    }
  }
  # Away team
  if(teams_MQS[[i, "Home_Away"]]=="Away"){
    # Add the opponent
    opponent <- c(opponent, teams_MQS[[i, "Home_Team"]])
    # Win
    if(teams_MQS[[i, "Home_Score"]] < teams_MQS[[i, "Away_Score"]]){
      W_L <- c(W_L, "W")
    }
    # Loss
    if(teams_MQS[[i, "Home_Score"]] > teams_MQS[[i, "Away_Score"]]){
      W_L <- c(W_L, "L")
    }
    # Draw
    if(teams_MQS[[i, "Home_Score"]] == teams_MQS[[i, "Away_Score"]]){
      W_L <- c(W_L, "D")
    }
  }
}

teams_MQS["W_L_D"] <- W_L
teams_MQS["Opponent"] <- opponent

################################################################################
################################ VISUALIZATIONS ################################
################################################################################

###################################################
##### BEST GAMES IN THE LAST FIVE UCL SEASONS #####
###################################################

# Get data
best_matches_data <- matches_MQS %>% 
  arrange(-MQS) %>% 
  mutate(Match = paste0(H_Team, " - ", A_Team, " (", season, " ", Matchweek, 
                        ")"),
         Score = paste0(" ", Score, " "),
         MQS = paste0(" ", round(MQS,2)), " ") %>% 
  select(Match, Score, MQS) %>% 
  head(10)

# Plot
best_games <- best_matches_data %>% 
  gt() %>% 
  # Add title and subtitle
  tab_header(title = md("**Games with the highest MQS**"),
             subtitle = "Champions League seasons 2020-21 to 2024-25") %>%
  # Set column names weights to bold
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(Match, Score, MQS))
  ) %>%
  # Set MQS values to bold
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(MQS))) %>%
  # Set columns widths
  cols_width(
    Match ~ px(500),
    Score ~ px(80),
    MQS ~ px(80)
  ) %>%
  # Center Score and MQS columns names
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(Score, MQS))
  )  %>%
  # Center Score and MQS content
  cols_align(
    align = "center",
    columns = c(Score, MQS)
  ) %>% 
  # Change table content font
  tab_style(
    style = cell_text(font = google_font("Libre Franklin")),
    locations = cells_body(columns = everything())
  ) %>%
  # Change column names font
  tab_style(
    style = cell_text(font = google_font("Libre Franklin")),
    locations = cells_column_labels(columns = everything())
  ) %>%
  # Change title and subtitle font
  tab_style(
    style = cell_text(font = google_font("Libre Franklin")),
    locations = cells_title(groups = c("title", "subtitle"))
  ) %>%
  # Change background color
  tab_options(
    table.background.color = "#fff4ec"
  )

# Save the table
gtsave(best_games, "visualizations/UCL_best_games/UCL_best_games.html")

# Transform the table from html to pdf
chrome_print("visualizations/UCL_best_games/UCL_best_games.html", 
             "visualizations/UCL_best_games/UCL_best_games.pdf",
             options = list(paperWidth = 14, paperHeight = 8))

############################################################
##### BEST TEAMS PERFORMANCES IN THE LAST FIVE SEASONS #####
############################################################

best_performances <- teams_MQS %>% 
  arrange(-team_MQS) %>% 
  mutate(Match=paste0(season, " ", Matchweek, ", ", Home_Team," - ", Away_Team, 
                      " (",Home_Score, " - ", Away_Score, ")"),
         TMQS=round(team_MQS, 2)) %>%
  select(Team, Match, TMQS) %>% 
  head(10) %>%
  gt() %>% 
  tab_header(title = md("**Teams with the highest TMQS**"),
             subtitle = "Champions League seasons 2020-21 to 2024-25") %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = c(Team, Match, TMQS))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(Team, TMQS))) %>% 
  # Set columns widths
  cols_width(
    Team ~ px(200),
    Match ~ px(550),
    TMQS ~ px(80)
  ) %>%
  # Center TMQS column name
  tab_style(
    style = cell_text(align = "center"), 
    locations = cells_column_labels(columns = c(TMQS))
  )  %>%
  # Center Score and MQS content
  cols_align(
    align = "center",
    columns = c(TMQS)
  ) %>% 
  # Change table content font
  tab_style(
    style = cell_text(font = google_font("Libre Franklin")),
    locations = cells_body(columns = everything())
  ) %>%
  # Change column names font
  tab_style(
    style = cell_text(font = google_font("Libre Franklin")),
    locations = cells_column_labels(columns = everything())
  ) %>%
  # Change title and subtitle font
  tab_style(
    style = cell_text(font = google_font("Libre Franklin")),
    locations = cells_title(groups = c("title", "subtitle"))
  ) %>%
  # Change background color
  tab_options(
    table.background.color = "#fff4ec"
  )

# Save the table
gtsave(best_performances, 
       "visualizations/UCL_best_performances/UCL_best_performances.html")

# Transform the table from html to pdf
chrome_print("visualizations/UCL_best_performances/UCL_best_performances.html", 
             "visualizations/UCL_best_performances/UCL_best_performances.pdf",
             options = list(paperWidth = 14, paperHeight = 8))

############################
##### UCL WINNERS RUNS #####
############################

plot_champions_TMQS <- function(d, t, s){
  # Filter the data
  champion_run_data <- d %>% 
    filter((Team==t)&(season==s)) %>% 
    mutate(Match=paste0(W_L_D, " ", Home_Score, " - ", Away_Score, " vs ", 
                        Opponent)) %>%
    arrange(Match_Date)
  
  # Get the variables for the plot
  stages_limits <- list()
  for(i in seq(nrow(champion_run_data))){
    # print(champion_run_data[i,])
    # Get the stage of the competition
    stage <- champion_run_data[[i, "Matchweek"]]
    if(!(stage %in% names(stages_limits))){
      stages_limits[[stage]] = 1
    }else{
      stages_limits[[stage]] = stages_limits[[stage]] + 1
    }
  }
  
  new_stages_limits <- list()
  for(i in seq(names(stages_limits))){
    new_stages_limits[[names(stages_limits)[i]]] = sum(unlist(stages_limits[1:i])) + 0.5
  }
  
  if(!("Knockout phase play-offs" %in% names(new_stages_limits))){
    KO_PR <- new_stages_limits[[names(new_stages_limits)[1]]]
  }else{
    KO_PR <- new_stages_limits[[names(new_stages_limits)[2]]]
    new_stages_limits[[names(new_stages_limits)[2]]] <- NULL
  }
  
  # Make the plot
  champion_run_plot <- ggplot() +
    annotate("rect",
             xmin = 0, xmax = new_stages_limits[[names(new_stages_limits)[1]]],
             ymin = -Inf, ymax = Inf,
             alpha = 0.2, fill = "green") +
    # Add Knockout phase play-offs background color
    annotate("rect",
             xmin = new_stages_limits[[names(new_stages_limits)[1]]], 
             xmax = KO_PR, ymin = -Inf, ymax = Inf, alpha = 0.2, 
             fill = "lightblue") +
    # Add round of 16 background color
    annotate("rect",
             xmin = KO_PR, 
             xmax = new_stages_limits[[names(new_stages_limits)[2]]],
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightyellow") +
    # Add quarter final background color
    annotate("rect",
             xmin = new_stages_limits[[names(new_stages_limits)[2]]], 
             xmax = new_stages_limits[[names(new_stages_limits)[3]]],
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "yellow") +
    # Add semi final background color
    annotate("rect",
             xmin = new_stages_limits[[names(new_stages_limits)[3]]], 
             xmax = new_stages_limits[[names(new_stages_limits)[4]]],
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "orange") +
    # Add final background color
    annotate("rect",
             xmin = new_stages_limits[[names(new_stages_limits)[4]]], 
             xmax = new_stages_limits[[names(new_stages_limits)[5]]],
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red")  +
    # Add the vertical lines between the games and the scores
    geom_segment(aes(x = seq(nrow(champion_run_data)), 
                     xend = seq(nrow(champion_run_data)), y = 30, 
                     yend = champion_run_data[["team_MQS"]]), 
                 linetype = "dashed", color = "gray50", size = 0.25) +
    # Plot the line
    geom_line(aes(x= seq(nrow(champion_run_data)), 
                  y=champion_run_data[["team_MQS"]]), lwd=0.5, color="black") +
    # Plot the points
    geom_point(aes(x= seq(nrow(champion_run_data)), 
                   y=champion_run_data[["team_MQS"]]), shape = 21, size = 2, 
               color = "black", fill ="white", stroke=0.75, alpha=1) +
    # Add title
    ggtitle(paste0(s, " ", t, "'s TMQS evolution")) +
    theme_minimal()  +
    ylim(30, 90) +
    labs(y = "TMQS", x = "Match") +
    scale_x_continuous(
      breaks = seq(1, nrow(champion_run_data), 1),
      labels = champion_run_data[["Match"]]
    ) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 60, 
                                family = "libre_franklin", 
                                margin = margin(b = 20)),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, 
                                 family = "libre_franklin", size = 30),
      axis.text.y = element_text(size = 30),
      axis.title.y = element_text(face = "bold", family = "libre_franklin", 
                                  margin = margin(r = 20), size = 40),
      axis.title.x = element_text(face = "bold", family = "libre_franklin", 
                                  margin = margin(t = 20), size = 40),
      panel.background = element_rect(fill = "#fff4ec", colour = NA),
      plot.background  = element_rect(fill = "#fff4ec", colour = NA)
    )
  
  # RETURN
  return(champion_run_plot)
}

# Apply the function for Paris Saint-Germain (2024-25)
p <- plot_champions_TMQS(teams_MQS, "Paris Saint-Germain", "2024-2025")

# Save the plot
ggsave("visualizations/TMQS_evolution/24_25.png", plot = p, width = 12, 
       height = 6, dpi = 300)

##############################
##### UCL BIGGEST UPSETS #####
##############################

q3 <- quantile(teams_MQS[["team_MQS"]], 0.75)

# Home upsets
home_upsets <- matches_MQS %>% 
  filter((H_Team_MQS >= q3) & (Home_Score < Away_Score)) %>% 
  mutate(TMQS_diff = H_Team_MQS-A_Team_MQS) %>%
  arrange(-TMQS_diff)

away_upsets <-  matches_MQS %>% 
  filter((A_Team_MQS >= q3) & (Home_Score > Away_Score)) %>% 
  mutate(TMQS_diff = A_Team_MQS-H_Team_MQS) %>%
  arrange(-TMQS_diff)

upsets <- bind_rows(home_upsets, away_upsets)
biggest_upsets_plot <- upsets %>% 
  mutate(
    `Home Team` = H_Team,
    `HT TMQS` = round(H_Team_MQS, 2),
    `Away Team` = A_Team,
    `AT TMQS` = round(A_Team_MQS, 2),
    Round = paste0(season," ",Matchweek),
    Match=paste0(H_Team, " (", round(H_Team_MQS,2), " TMQS)", " - ", A_Team, 
                 " (", round(A_Team_MQS,2), " TMQS)")
  ) %>% 
  arrange(-TMQS_diff) %>%
  head(10) %>%
  mutate(` ` = rep("", 10),
         `  ` = rep("", 10),
         `   ` = rep("", 10)) %>%
  select(Round, ` `, `Home Team`, `HT TMQS`, `  `, `Away Team`, `AT TMQS`, 
         `   `, Score) %>% 
  gt() %>% 
  tab_header(title = md("**Champions League's biggest upsets based on TMQS**"),
             subtitle = "From 2020-21 to 2024-25 season") %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(`HT TMQS`, `AT TMQS`, `Score`))) %>% 
  # Center TMQS column name
  tab_style(
    style = cell_text(align = "center"), 
    locations = cells_column_labels(columns = c(`HT TMQS`, `AT TMQS`, Score))
  )  %>%
  # Center Score and MQS content
  cols_align(
    align = "center",
    columns = c(`HT TMQS`, `AT TMQS`, Score)
  ) %>%
  # Set columns widths
  cols_width(
    Round ~ px(230),
    `Home Team` ~ px(175),
    `HT TMQS` ~ px(90),
    `Away Team` ~ px(175),
    `AT TMQS` ~ px(90),
    `Score` ~ px(90),
    ` ` ~ px(75),
    `  ` ~ px(75),
    `   ` ~ px(75)
  ) %>% 
  # Change table content font
  tab_style(
    style = cell_text(font = google_font("Libre Franklin")),
    locations = cells_body(columns = everything())
  ) %>%
  # Change column names font
  tab_style(
    style = cell_text(font = google_font("Libre Franklin")),
    locations = cells_column_labels(columns = everything())
  ) %>%
  # Change title and subtitle font
  tab_style(
    style = cell_text(font = google_font("Libre Franklin")),
    locations = cells_title(groups = c("title", "subtitle"))
  ) %>%
  # Change background color
  tab_options(
    table.background.color = "#fff4ec"
  )

# Save the table
gtsave(biggest_upsets_plot, 
       "visualizations/UCL_biggest_upsets/UCL_biggest_upsets.html")

# Transform the table from html to pdf
chrome_print("visualizations/UCL_biggest_upsets/UCL_biggest_upsets.html", 
             "visualizations/UCL_biggest_upsets/UCL_biggest_upsets.pdf", 
             options = list(paperWidth = 14, paperHeight = 8))

#########################################################
##### AVERAGE MQS AT EVERY STAGE OF THE COMPETITION #####
#########################################################

# Set up the dataframe
stages_mean_MQS <- matches_MQS %>% 
  group_by(season, Matchweek) %>% 
  summarise(stage_mean_MQS = mean(MQS, na.rm=TRUE))

# Add the number of the stage
stage_nb <- list("Group stage"=1,
                 "League phase"=1,
                 "Knockout phase play-offs"=2,
                 "Round of 16"=3,
                 "Quarter-finals"=4,
                 "Semi-finals"=5,
                 "Final"=6)

# Asses the right value
stage_nb_col <- c()
for(i in stages_mean_MQS[["Matchweek"]]){
  stage_nb_col <- c(stage_nb_col, stage_nb[[i]])
}

# Add the column to the dataframe
stages_mean_MQS$Matchweek_nb <- stage_nb_col

# Stages labels
stages_labels <- c("Group stage/\nLeague phase", "Knockout\nphase play-offs", 
                   "Round of 16", "Quarter-finals", "Semi-finals", "Final")

p_change_mqs <- ggplot() +
  # Add vertical dashed lines at every stage
  geom_segment(aes(x = seq(6), xend = seq(6), y = 40, yend = 65), 
               linetype = "dashed", color = "gray50", size = 0.5) +
  # 2020-2021
  geom_line(data = stages_mean_MQS %>% filter(season=="2020-2021") %>% 
              arrange(Matchweek_nb), aes(x=Matchweek_nb, y=stage_mean_MQS), 
            color="black", lwd=0.5) +
  geom_point(data = stages_mean_MQS %>% filter(season=="2020-2021") %>% 
               arrange(Matchweek_nb), aes(x=Matchweek_nb, y=stage_mean_MQS), 
             color="black", shape = 21, size = 2, fill ="white", stroke=0.75) +
  # 2021-2022
  geom_line(data = stages_mean_MQS %>% filter(season=="2021-2022") %>% 
              arrange(Matchweek_nb), aes(x=Matchweek_nb, y=stage_mean_MQS), 
            color="red", lwd=0.5) +
  geom_point(data = stages_mean_MQS %>% filter(season=="2021-2022") %>% 
               arrange(Matchweek_nb), aes(x=Matchweek_nb, y=stage_mean_MQS), 
             color="red", shape = 21, size = 2, fill ="white", stroke=0.75) +
  # 2022-2023
  geom_line(data = stages_mean_MQS %>% filter(season=="2022-2023") %>% 
              arrange(Matchweek_nb), aes(x=Matchweek_nb, y=stage_mean_MQS), 
            color="orange", lwd=0.5) +
  geom_point(data = stages_mean_MQS %>% filter(season=="2022-2023") %>% 
               arrange(Matchweek_nb), aes(x=Matchweek_nb, y=stage_mean_MQS), 
             color="orange", shape = 21, size = 2, fill ="white", stroke=0.75) +
  # 2023-2024
  geom_line(data = stages_mean_MQS %>% filter(season=="2023-2024") %>% 
              arrange(Matchweek_nb), aes(x=Matchweek_nb, y=stage_mean_MQS), 
            color="blue", lwd=0.5) +
  geom_point(data = stages_mean_MQS %>% filter(season=="2023-2024") %>% 
               arrange(Matchweek_nb), aes(x=Matchweek_nb, y=stage_mean_MQS), 
             color="blue", shape = 21, size = 2, fill ="white", stroke=0.75) +
  # 2024-2025
  geom_line(data = stages_mean_MQS %>% filter(season=="2024-2025") %>% 
              arrange(Matchweek_nb), aes(x=Matchweek_nb, y=stage_mean_MQS), 
            color="forestgreen", lwd=0.5) +
  geom_point(data = stages_mean_MQS %>% filter(season=="2024-2025") %>% 
               arrange(Matchweek_nb), aes(x=Matchweek_nb, y=stage_mean_MQS), 
             color="forestgreen", shape = 21, size = 2, fill ="white", 
             stroke=0.75) + 
  # Set xlab
  xlab("Stage") +
  # Set ylab
  ylab("Stage's mean MQS") +
  # Set ylim
  ylim(40, 65) +
  # Set title
  ggtitle("Change in average MQS over the stages") +
  # Change x axe labels
  scale_x_continuous(
    breaks = seq(6),
    labels = stages_labels
  ) +
  # Theme minimal
  theme_minimal() +
  # Theme
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, 
                               family = "libre_franklin", size = 30, 
                               lineheight=0.3),
    axis.text.y = element_text(family = "libre_franklin", size = 30),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 60, 
                              family = "libre_franklin", 
                              margin = margin(b = 20)),
    axis.title.x = element_text(face = "bold", family = "libre_franklin", 
                                margin = margin(t = 10), size = 40),
    axis.title.y = element_text(face = "bold", family = "libre_franklin", 
                                margin = margin(r = 20), size = 40),
    panel.background = element_rect(fill = "#fff4ec", colour = NA),
    plot.background  = element_rect(fill = "#fff4ec", colour = NA)
  )

# Save the plot
ggsave("visualizations/Average_MQS/MQS_change.png", plot = p_change_mqs, 
       width = 12, height = 6, dpi = 300)

