
#FPL STAT ATTACK

#This file contains some data read ins that we want to share across UI and server.

library(shiny)
library(curl)
library(jsonlite)
library(dplyr)
library(knitr)
library(DT)
library(stringi)


##read in 2017/18 data
unlist_json_players <- read.csv("https://www.dropbox.com/s/de3pp2pgj06w2xs/player_data201718.csv?dl=1")

unlist_json_players_1819 <- read.csv("https://www.dropbox.com/s/cvso06zr4y8ibsk/player_data.csv?dl=1")

json_players_det <- read.csv("https://www.dropbox.com/s/nt9681eli4fdy1z/player_data_201718_2.csv?dl=1")

json_players_det_1819 <- read.csv("https://www.dropbox.com/s/jgm090pp7s0blxz/player_metadata.csv?dl=1")

json_teams <- read.csv("https://www.dropbox.com/s/t0uoq2gieq9xsaf/team_data.csv?dl=1")

json_players_det$web_name <- as.character(json_players_det$web_name)
json_players_det$web_name <- stri_encode(json_players_det$web_name, "", "UTF-8") 
json_players_det$web_name <- stri_trans_general(json_players_det$web_name, "Latin-ASCII")
json_players_det$web_name <- enc2native(json_players_det$web_name)


json_players_det_1819$web_name <- as.character(json_players_det_1819$web_name)
json_players_det_1819$web_name <- stri_encode(json_players_det_1819$web_name, "", "UTF-8") 
json_players_det_1819$web_name <- stri_trans_general(json_players_det_1819$web_name, "Latin-ASCII")
json_players_det_1819$web_name <- enc2native(json_players_det_1819$web_name)

##define some common v
var_names <- colnames(unlist_json_players)

var_labels <-c("x","newid","id","kickoff_time","kickoff_time_formatted","Home team score",
               "Away team score","Was home","round","Total points","Value","Transfers balance", 
               "Selected","Transfers in","Transfers out","Loaned in","Loaned out","Minutes",
               "Goals scored","Assists","Clean sheets","Goals conceded","Own goals","Penalties saved",
               "Penalties missed","Yellow cards","Red cards","Saves","Bonus points","Bonus points system",
               "Influence","Creativity","Threat","ICT index","ea_index", "open play crosses", "big chances created",
               "Clearances blocks interceptions","Recoveries",
               "Key passes","tackles","winning_goals", "attempted_passes",
               "Completed passes","Penalties conceded","Big chances missed","Errors leading to goal",
               "Errors leading to goal attempts","Tackled","Offsides","Target missed","Fouls","Dribbles","element",
               "fixture","opponent_team")

var_name_labels <-cbind(var_names,var_labels)

var_labels_out <- c("Total points","Minutes","Influence","Creativity","Threat","ICT index","Bonus points",
                    "Bonus points system","Selected", "Transfers balance", 
                   "Transfers in","Transfers out", "Goals scored","Assists","Clean sheets","Goals conceded","Own goals","Penalties saved",
                   "Penalties missed","Yellow cards","Red cards","Saves",
                   "open play crosses", "big chances created",
                   "Clearances blocks interceptions","Recoveries",
                   "Key passes","tackles", "Completed passes","Penalties conceded","Big chances missed","Errors leading to goal",
                   "Errors leading to goal attempts","Tackled","Offsides","Target missed","Fouls","Dribbles")

# Define server logic to load and filter the data
fpl_team_listx <- as.data.frame(c("Arsenal","Bournemouth","Brighton","Burnley","Chelsea","Crystal Palace","Everton",
                                  "Huddersfield","Leicester","Liverpool","Man City","Man Utd","Newcastle","Southampton",
                                  "Stoke","Swansea","Tottenham","Watford","West Brom","West Ham"))
fpl_team_list <- cbind(fpl_team_listx,c(1:20))
colnames(fpl_team_list) <- c("team_name","team")

##2018/19 season teams
fpl_team_listx18 <- as.data.frame(c("Arsenal","Bournemouth","Brighton","Burnley","Cardiff","Chelsea","Crystal Palace","Everton",
                                  "Fulham","Huddersfield","Leicester","Liverpool","Man City","Man Utd","Newcastle","Southampton",
                                  "Tottenham","Watford","West Ham","Wolves"))
fpl_team_list18 <- cbind(fpl_team_listx18,c(1:20))
colnames(fpl_team_list18) <- c("team_name","team")

position_listx <- as.data.frame(c("Goalkeeper","Defender","Midfielder","Forward"))
position_list <- cbind(position_listx,c(1:4))
colnames(position_list) <- c("position","element_type")
position_listy <- c("None","Goalkeeper","Defender","Midfielder","Forward")

team_filter_1718 <- c("None","Arsenal","Bournemouth","Brighton","Burnley","Chelsea","Crystal Palace","Everton",
                      "Huddersfield","Leicester","Liverpool","Man City","Man Utd","Newcastle","Southampton",
                      "Stoke","Swansea","Tottenham","Watford","West Brom","West Ham")

team_filter <- c("None","Arsenal","Bournemouth","Brighton","Burnley","Cardiff","Chelsea","Crystal Palace","Everton",
  "Fulham","Huddersfield","Leicester","Liverpool","Man City","Man Utd","Newcastle","Southampton",
  "Tottenham","Watford","West Ham","Wolves")

fpl_team_list182 <- fpl_team_list18
colnames(fpl_team_list182) <- c("opponent_name","opponent")

#read in player predictions

player_preds <- read.csv("https://www.dropbox.com/s/4xvfnzyeyt6tbe4/player_predictions.csv?dl=1")

