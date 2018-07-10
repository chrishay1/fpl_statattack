#
# This is the server logic for the FPl stat attack Shiny application. 
# Written by; Christopher Hay
#
# 
# 
#    
#
#json_player_list <- list(0)
library(shiny)
library(curl)
library(jsonlite)
library(dplyr)
json_input <- fromJSON("https://fantasy.premierleague.com/drf/element-summary/1")
json_input_hist<-json_input$history

var_names <- colnames(json_input$history)

var_labels <-c("id","kickoff_time","kickoff_time_formatted","Home team score",
               "Away team score","Was home","round","Total points","Value","Transfers balance",                                   "Selected","Transfers in","Transfers out","Loaned in","Loaned out","Minutes",
               "Goals scored","Assists","Clean sheets","Goals conceded","Own goals","Penalties saved",
               "Penalties missed","Yellow cards","Red cards","Saves","Bonus points","Bonus points system",
               "Influence","Creativity","Threat","ICT index","ea_index", "open play crosses", "big chances created",
               "Clearances blocks interceptions","Recoveries",
               "Key passes","tackles","winning_goals", "attempted_passes",
               "Completed passes","Penalties conceded","Big chances missed","Errors leading to goal",
               "Errors leading to goal attempts","Tackled","Offsides","Target missed","Fouls","Dribbles","element",
               "fixture","opponent_team")

unlist_json_players <- read.csv("https://www.dropbox.com/s/cvso06zr4y8ibsk/player_data.csv?dl=1")
json_players <-fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")
json_players_det <- json_players$elements
json_teams <- read.csv("https://www.dropbox.com/s/t0uoq2gieq9xsaf/team_data.csv?dl=1")
# Define server logic to load and filter the data
fpl_team_listx <- as.data.frame(c("Arsenal","Bournemouth","Brighton","Burnley","Chelsea","Crystal Palace","Everton",
                                  "Huddersfield","Leicester","Liverpool","Man City","Man Utd","Newcastle","Southampton",
                                  "Stoke","Swansea","Tottenham","Watford","West Brom","West Ham"))
fpl_team_list <- cbind(fpl_team_listx,c(1:20))
colnames(fpl_team_list) <- c("team_name","team")

shinyServer(function(input, output) {
    
    stat_table <- reactive({
        
        fpl_stat_use <-var_names[match(input$stat,var_labels)]
        

        
        #list of positions
        position_listx <- as.data.frame(c("Goalkeeper","Defender","Midfielder","Forward"))
        position_list <- cbind(position_listx,c(1:4))
        colnames(position_list) <- c("position","element_type")
        
        
        nrow(unlist_json_players)
        
        
        fpl_start <- input$min_gw[1]
        fpl_end <- input$min_gw[2]
        
        player_data2 <- unlist_json_players[unlist_json_players$round>=fpl_start&unlist_json_players$round<=fpl_end,]
        
        player_team_index <- json_players_det[,c("team","id","web_name","element_type","now_cost")]
        colnames(player_team_index)[2] <-c("newid")
        colnames(player_team_index)[5] <-c("cost")
        player_team_index$cost <- player_team_index$cost/10
        player_data_x <- player_data2[,c("newid",fpl_stat_use)]
        colnames(player_data_x) <- c("newid","stat")
        player_data3 <- player_data_x %>% group_by(newid) %>% summarise(mean = mean(stat),total=sum(stat)) %>% 
            arrange(desc(mean)) %>% inner_join(player_team_index) %>% inner_join(fpl_team_list) %>%
            inner_join(position_list)
        colnames(player_data3)[2] <- paste0("Average ",fpl_stat_use)
        colnames(player_data3)[3] <- paste0("Total ",fpl_stat_use)
       player_data4 <- player_data3[,-c(1,4,6)]
       player_data5 <- player_data4[,c(3,4,5,6,1,2)]
        colnames(player_data5)[c(1:3)] <- c("player name","cost","team name","position")
        player_data6 <- player_data5[player_data5$cost >=input$cost[1]&player_data5$cost <= input$cost[2],]
        if (input$pos != ""){
            player_data6 <- player_data6[player_data6$position==input$pos,]   
        }
        
        print(player_data6[1:input$no_players,])
    })  
    output$fpl_table <- renderTable(stat_table())

    team_table <- reactive({
        json_teams<- json_teams[json_teams$gw>=input$team_gw[1]&json_teams$gw<=input$team_gw[2],]
        home_diff <- json_teams %>% group_by(home_team) %>% summarise(home_diff_sum=sum(home_diff),home_count=n())
        away_diff <- json_teams %>% group_by(away_team) %>% summarise(away_diff_sum=sum(away_diff),away_count=n())
        total_diff <- full_join(home_diff,away_diff,by=c("home_team"="away_team")) 
        total_diff[is.na(total_diff)] <-0
        total_diff <- total_diff %>% 
            mutate(total_diff_sum=home_diff_sum+away_diff_sum,total_count=home_count+away_count)%>%
            mutate(diff_rank = total_diff_sum / total_count)
        colnames(total_diff)[1] <- c("team")
        total_diff <- full_join(total_diff,fpl_team_list) %>% arrange(diff_rank)
        total_diff_x <- total_diff[,c("team_name","diff_rank","total_count","home_count","away_count")]
        colnames(total_diff_x) <- c("Team name","Average difficulty ranking","Total count","Home game count","Away games count")
        total_diff_x[is.na(total_diff_x)] <- 0
        return(total_diff_x)
        
    })
    output$team_table <- renderTable(team_table())
})
    
 
  

