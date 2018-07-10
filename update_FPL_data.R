##This R scripts scrapes the FPL data and puts it into a nice CSV file that we can point the 
##shiny app at

##needs library(jsonlite)  !

##Useful links;
##https://www.reddit.com/r/FantasyPL/comments/5q59h8/fantasypl_api/
##https://www.reddit.com/r/FantasyPL/comments/4tpd2r/an_updated_link_for_the_fantasypl_web_api/


library(jsonlite)
library(dplyr)
json_players <-fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")
json_players_det <- json_players$elements
###download the data from the FPL website
json_player_list <- list()
for (i in 1:nrow(json_players_det)) {
    json_player <- fromJSON(paste0("https://fantasy.premierleague.com/drf/element-summary/",i))
    json_player_list[[i]]<-json_player$history
}




unlist_json_players <- bind_rows(json_player_list,.id="newid")

unlist_json_players$influence <- as.numeric(unlist_json_players$influence)
unlist_json_players$creativity <- as.numeric(unlist_json_players$creativity)
unlist_json_players$threat <- as.numeric(unlist_json_players$threat)
unlist_json_players$newid <- as.numeric(unlist_json_players$newid)
unlist_json_players$ict_index <- as.numeric(unlist_json_players$ict_index)

# colnames(unlist_json_players) <-c("newid","id","kickoff_time","kickoff_time_formatted","Home team score",
#                                    "Away team score","Was home","round","Total points","Value","Transfers balance",
#                                    "Selected","Transfers in","Transfers out","Loaned in","Loaned out","Minutes",
#                                    "Goals scored","Assists","Clean sheets","Goals conceded","Own goals","Penalties saved",
#                                    "Penalties missed","Yellow cards","Red cards","Saves","Bonus points","Bonus points system",
#                                    "Influence","Creativity","Threat","ICT index","Clearances blocks interceptions","Recoveries",
#                                    "Key passes","Completed passes","Penalties conceded","Big chances missed","Errors leading to goal",
#                                    "Errors leading to goal attempts","Tackled","Offsides","Target missed","Fouls","Dribbles","element",
#                                    "fixture","opponent_team")
write.csv(unlist_json_players,"C:/Users/pc/Dropbox/FPLData/player_data.csv")

#teams

fpl_team_listx <- as.data.frame(c("Arsenal","Bournemouth","Brighton","Burnley","Chelsea","Crystal Palace","Everton",
                                  "Huddersfield","Leicester","Liverpool","Man City","Man Utd","Newcastle","Southampton",
                                  "Stoke","Swansea","Tottenham","Watford","West Brom","west Ham"))
fpl_team_list <- cbind(fpl_team_listx,c(1:20))
colnames(fpl_team_list) <- c("team_name","team")
fixtures <- list()
for(j in c(1:38)){
    json_fixtures <- fromJSON(paste0("https://fantasy.premierleague.com/drf/fixtures/?event=",j))
    fixtures_x <- as.data.frame(cbind(json_fixtures$team_h,json_fixtures$team_a,json_fixtures$team_h_difficulty,json_fixtures$team_a_difficulty))
    fixtures_x$gw <- c(rep(j,nrow(fixtures_x))) 
    fixtures[[j]] <- fixtures_x
    
}

unlist_fixtures <- bind_rows(fixtures,.id="id")
colnames(unlist_fixtures)[2:5] <- c("home_team","away_team","home_diff","away_diff")
write.csv(unlist_fixtures,"C:/Users/pc/Dropbox/FPLData/team_data.csv")

