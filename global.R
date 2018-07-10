
#This file contains some data read ins that we want to share across UI and server.

library(shiny)
library(curl)
library(jsonlite)
library(dplyr)
library(knitr)
library(DT)


##read in 2017/18 data
unlist_json_players <- read.csv("https://www.dropbox.com/s/de3pp2pgj06w2xs/player_data201718.csv?dl=1")
json_players_det <- read.csv("https://www.dropbox.com/s/xsdb07qqx2j8f9x/player_metadata%20201718.csv?dl=1")
json_teams <- read.csv("https://www.dropbox.com/s/kffr8yh2qo8gyz3/team_data%20201718.csv?dl=1")


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
# Define server logic to load and filter the data
fpl_team_listx <- as.data.frame(c("Arsenal","Bournemouth","Brighton","Burnley","Chelsea","Crystal Palace","Everton",
                                  "Huddersfield","Leicester","Liverpool","Man City","Man Utd","Newcastle","Southampton",
                                  "Stoke","Swansea","Tottenham","Watford","West Brom","West Ham"))
fpl_team_list <- cbind(fpl_team_listx,c(1:20))
colnames(fpl_team_list) <- c("team_name","team")

position_listx <- as.data.frame(c("Goalkeeper","Defender","Midfielder","Forward"))
position_list <- cbind(position_listx,c(1:4))
colnames(position_list) <- c("position","element_type")
position_listy <- c("","Goalkeeper","Defender","Midfielder","Forward")